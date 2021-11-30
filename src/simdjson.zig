const std = @import("std");
const mem = std.mem;
const log = std.log.debug;

const u8x64 = std.meta.Vector(64, u8);
const u8x32 = std.meta.Vector(32, u8);
const u64x2 = std.meta.Vector(2, u64);
const step_size = 64;
const Log2StepSizeInt = std.meta.Int(.unsigned, std.math.log2_int(u8, step_size));

pub const Options = struct {
    /// allocator to use when pointer or slice fields are found
    allocator: *mem.Allocator,

    /// if false, unknown fields will cause an error.UnknownField
    /// otherwise they will be skipped. defaults to true.
    skip_unknown_fields: bool = true,
};

/// helper around `parseUnbuffered` with a `std.io.bufferedReader(reader)`.
/// if T includes methods such as `on_field` 
/// with the following signature, they will be called when `field` is found: 
///   `pub fn on_field(reader: anytype, allocator: *mem.Allocator, p: *Parser) !Ret`.
/// the return type Ret must coerce to the type of `field`.
pub fn parse(comptime T: type, reader: anytype, options: Options) !T {
    var buffered_reader = std.io.bufferedReader(reader);
    return parseUnbuffered(T, &buffered_reader, options);
}

/// same as `parse` but doesn't make a buffered reader from `reader`
pub fn parseUnbuffered(comptime T: type, reader: anytype, options: Options) !T {
    var p = try parser(options.allocator, reader);
    defer p.deinit();
    return p.parseImpl(T, options);
}

/// helper around `parse()` via `io.fixedBufferStream(input).reader()`
pub fn parseText(comptime T: type, input: []const u8, options: Options) !T {
    var fbs = std.io.fixedBufferStream(input);
    return try parse(T, fbs.reader(), options);
}

/// Releases resources created by `parse`.
/// Should be called with the same type and `ParseOptions` that were passed to `parse`
pub fn parseFree(comptime T: type, value: T, options: Options) void {
    switch (@typeInfo(T)) {
        .Bool, .Float, .ComptimeFloat, .Int, .ComptimeInt, .Enum => {},
        .Optional => if (value) |v| return parseFree(@TypeOf(v), v, options),
        .Union => |unionInfo| {
            if (unionInfo.tag_type) |UnionTagType| {
                inline for (unionInfo.fields) |u_field| {
                    if (value == @field(UnionTagType, u_field.name)) {
                        parseFree(u_field.field_type, @field(value, u_field.name), options);
                        break;
                    }
                }
            }
        },
        .Struct => |structInfo| {
            inline for (structInfo.fields) |field| {
                if (!field.is_comptime)
                    parseFree(field.field_type, @field(value, field.name), options);
            }
        },
        .Array => |arrayInfo| {
            for (value) |v|
                parseFree(arrayInfo.child, v, options);
        },
        .Pointer => |ptrInfo| {
            switch (ptrInfo.size) {
                .One => {
                    parseFree(ptrInfo.child, value.*, options);
                    options.allocator.destroy(value);
                },
                .Slice => {
                    for (value) |v|
                        parseFree(ptrInfo.child, v, options);
                    options.allocator.free(value);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

/// initializes a Parser(Reader) and loads a block
pub fn parser(allocator: *mem.Allocator, reader: anytype) !Parser(@TypeOf(reader)) {
    var p: Parser(@TypeOf(reader)) = .{
        .allocator = allocator,
        .block = undefined,
        .reader = reader,
    };
    p.block = try p.nextBlock();
    return p;
}

const StringBlock = struct {
    backslash: u64,
    escaped: u64,
    quote: u64,
    in_string: u64,

    inline fn string_tail(sb: StringBlock) u64 {
        return sb.in_string ^ sb.quote;
    }

    inline fn non_quote_inside_string(sb: StringBlock, mask: u64) u64 {
        return mask & sb.in_string;
    }

    fn dump(sb: StringBlock) void {
        log("StringBlock ", .{});
        log("{b:0>64}: backslash", .{@bitReverse(u64, sb.backslash)});
        log("{b:0>64}: escaped", .{@bitReverse(u64, sb.escaped)});
        log("{b:0>64}: quote", .{@bitReverse(u64, sb.quote)});
        log("{b:0>64}: in_string", .{@bitReverse(u64, sb.in_string)});
    }
};

extern fn @"llvm.x86.pclmulqdq"(u64x2, u64x2, i8) u64x2;
inline fn carrylessMul(a: u64x2, b: u64x2) u64x2 {
    return @bitCast(
        u64x2,
        @"llvm.x86.pclmulqdq"(a, b, 0),
    );
}

pub fn Parser(comptime Reader: type) type {
    if (!std.meta.trait.is(.Pointer)(Reader))
        @compileError(comptime std.fmt.comptimePrint("reader must be a pointer. found type '{s}'", .{@typeName(Reader)}));

    return struct {
        allocator: *mem.Allocator,
        prev_escaped: u64 = 0,
        prev_scalar: u64 = 0,
        prev_in_string: u64 = 0,
        block: Block,
        /// the current index within input_buf
        index: Log2StepSizeInt = 0,
        input_buf: [step_size]u8 = undefined,
        /// temporary buffer for keys and string values
        string_buf: std.ArrayListUnmanaged(u8) = .{},
        /// temporary buffer for numbers, true, false and null tokens
        token_buf: [40]u8 = undefined,
        /// single character lookbehind
        last_char: ?u8 = null,
        reader: Reader,

        const Self = @This();
        pub fn init(allocator: *mem.Allocator, reader: Reader) !Self {
            var p: Self = .{
                .allocator = allocator,
                .block = undefined,
            };
            p.block = try p.nextBlock(reader);
            return p;
        }
        pub fn deinit(p: *Self) void {
            p.string_buf.deinit(p.allocator);
        }

        inline fn nextStringBlock(self: *Self, input_vec: u8x64) StringBlock {
            const backslash_vec = input_vec == @splat(64, @as(u8, '\\'));
            const backslash = @bitCast(u64, backslash_vec);
            const escaped = self.find_escaped(backslash);
            const quote_vec = input_vec == @splat(64, @as(u8, '"'));
            const quote = @bitCast(u64, quote_vec) & ~escaped;

            //
            // prefix_xor flips on bits inside the string (and flips off the end quote).
            //
            // Then we xor with prev_in_string: if we were in a string already, its effect is flipped
            // (characters inside strings are outside, and characters outside strings are inside).
            //
            const ones: u64x2 = [1]u64{std.math.maxInt(u64)} ** 2;
            var in_string = carrylessMul(.{ quote, 0 }, ones)[0];
            // println("{b:0>64} | quote a", .{@bitReverse(u64, quote)});
            // println("{b:0>64} | ones[0]", .{@bitReverse(u64, ones[0])});
            // println("{b:0>64} | in_string a", .{@bitReverse(u64, in_string)});
            // println("{b:0>64} | prev_in_string a", .{@bitReverse(u64, parser.prev_in_string)});
            in_string ^= self.prev_in_string;
            // println("{b:0>64} | in_string b", .{@bitReverse(u64, in_string)});

            //
            // Check if we're still in a string at the end of the box so the next block will know
            //
            // right shift of a signed value expected to be well-defined and standard
            // compliant as of C++20, John Regher from Utah U. says this is fine code
            //
            // println("{b:0>64} | prev_in_string a", .{@bitReverse(u64, parser.prev_in_string)});
            // println("{b:0>64} | @bitCast(i64, in_string) ", .{@bitReverse(i64, @bitCast(i64, in_string))});
            // println("{b:0>64} | @bitCast(i64, in_string) >> 63 ", .{@bitReverse(i64, @bitCast(i64, in_string) >> 63)});
            // println("{b:0>64} | @bitCast(u64, @bitCast(i64, in_string) >> 63) ", .{@bitReverse(u64, @bitCast(u64, @bitCast(i64, in_string) >> 63))});
            self.prev_in_string = @bitCast(u64, @bitCast(i64, in_string) >> 63);

            // Use ^ to turn the beginning quote off, and the end quote on.

            // We are returning a function-local object so either we get a move constructor
            // or we get copy elision.
            return StringBlock{
                .backslash = backslash,
                .escaped = escaped,
                .quote = quote,
                .in_string = in_string,
            };
        }

        inline fn follows(match: u64, overflow: *u64) u64 {
            const result = match << 1 | overflow.*;
            overflow.* = match >> 63;
            return result;
        }

        pub fn nextBlock(self: *Self) !Block {
            self.input_buf = [1]u8{0x20} ** step_size;
            self.index = 0;
            const n = try self.reader.read(&self.input_buf);
            if (n == 0) return error.EndOfStream;
            const string = self.nextStringBlock(self.input_buf);
            // identifies the white-space and the structurat characters
            const characters = CharacterBlock.classify(self.input_buf);

            // The term "scalar" refers to anything except structural characters and white space
            // (so letters, numbers, quotes).
            // We want  follows_scalar to mark anything that follows a non-quote scalar (so letters and numbers).
            //
            // A terminal quote should either be followed by a structural character (comma, brace, bracket, colon)
            // or nothing. However, we still want ' "a string"true ' to mark the 't' of 'true' as a potential
            // pseudo-structural character just like we would if we had  ' "a string" true '; otherwise we
            // may need to add an extra check when parsing strings.
            //
            // Performance: there are many ways to skin this cat.
            const nonquote_scalar = characters.scalar() & ~string.quote;
            const follows_nonquote_scalar = follows(nonquote_scalar, &self.prev_scalar);
            // if (unescaped & strings.in_string != 0) return error.UnescapedCharacters;
            return Block{
                .string = string, // strings is a function-local object so either it moves or the copy is elided.
                .characters = characters,
                .follows_nonquote_scalar = follows_nonquote_scalar,
            };
        }

        pub fn nextChar(self: *Self) !u8 {
            if (self.last_char) |lc| {
                log("nextChar() using last_char '{c}'", .{lc});
                defer self.last_char = null;
                return lc;
            }
            while (true) {
                log("nextChar() index {}", .{self.index});
                // parser.dump();

                log("{b:0>64} : quote", .{@bitReverse(u64, self.block.string.quote >> self.index << self.index)});
                const index = std.math.min(
                    @ctz(u64, self.block.string.quote >> self.index << self.index),
                    @ctz(u64, self.block.characters.op >> self.index << self.index),
                );
                log("nextChar() index2 {}", .{index});
                if (index < step_size) {
                    const new_index = @truncate(Log2StepSizeInt, index);
                    const result = self.input_buf[new_index];
                    log("nextChar result '{c}'", .{result});
                    if (new_index == step_size - 1) {
                        self.block = try self.nextBlock();
                    } else self.index = new_index + 1;
                    return result;
                }
                self.block = try self.nextBlock();
            }
            unreachable;
        }

        pub fn nextToken(self: *Self, comptime max_len: u32) ![]const u8 {
            log("nextToken index {}", .{self.index});
            try self.skipWs();
            // log("nextToken after skipWs index {}", .{self.index});
            // self.dump();
            const scalar = (self.block.characters.whitespace | self.block.characters.op) >> self.index;
            const token_len = if (scalar == 0) step_size - @as(u7, self.index) else @ctz(u64, scalar);
            log("{b:0>64} : scalar, token_len {}", .{ @bitReverse(u64, scalar), token_len });
            const token_end = self.index + std.math.min(token_len, max_len);
            // const token_end = self.index + token_len;
            log("{b:0>64} : scalar, token_len {} token_end {}", .{ @bitReverse(u64, scalar), token_len, token_end });
            const slice = if (token_end < step_size) blk: {
                mem.copy(u8, &self.token_buf, self.input_buf[self.index..token_end]);
                break :blk self.token_buf[0..token_len];
            } else {
                log("nextToken index 2 {}", .{self.index});
                mem.copy(u8, &self.token_buf, self.input_buf[self.index..]);
                self.block = try self.nextBlock();
                // self.dump();
                const scalar2 = (self.block.characters.whitespace | self.block.characters.op);
                const token_len2 = @ctz(u64, scalar2);
                log("{b:0>64} : scalar2, token_len2 {}", .{ @bitReverse(u64, scalar2), token_len2 });
                const slice2 = if (token_len2 < step_size)
                    self.input_buf[0..token_len2]
                else
                    return error.TokenTooLong;
                mem.copy(u8, self.token_buf[token_len..], slice2);
                self.index = @truncate(Log2StepSizeInt, token_len2);
                return self.token_buf[0..std.math.min(token_len + token_len2, max_len)];
            };
            log("nextToken slice '{s}'", .{slice});
            self.index += @truncate(Log2StepSizeInt, slice.len);
            return slice;
        }

        pub fn expectChar(self: *Self, comptime expected: u8) !void {
            log("expectChar('{c}')", .{expected});
            const actual = try self.nextChar();
            if (expected != actual) {
                std.log.err("expecting '{c}' but found '{c}'", .{ expected, actual });
                return error.UnexpectedChar;
            }
        }

        pub fn skipWs(self: *Self) !void {
            const mask = @as(u64, 1) << self.index;
            // log("{b:0>64}: mask", .{@bitReverse(u64, mask)});
            // log("{b:0>64}: self.block.characters.whitespace", .{@bitReverse(u64, self.block.characters.whitespace)});

            const is_next_ws = @boolToInt(mask & self.block.characters.whitespace != 0);
            self.index += is_next_ws * try std.math.cast(Log2StepSizeInt, @ctz(u64, ~self.block.characters.whitespace >> self.index));
            // log("skipWs new index {}", .{self.index});
        }

        pub fn incIndex(self: *Self) !void {
            if (@addWithOverflow(Log2StepSizeInt, self.index, 1, &self.index))
                self.block = try self.nextBlock();
        }

        /// assumes self.index is one past leading '"'
        pub fn readString(self: *Self) ![]const u8 {
            self.string_buf.items.len = 0;
            while (true) {
                const start = self.index;
                const new_index = @ctz(u64, self.block.string.quote >> self.index << self.index);
                try self.string_buf.appendSlice(self.allocator, self.input_buf[start..new_index]);
                log("readString start {} new_index {} string_buf '{s}'", .{ start, new_index, self.string_buf.items });
                if (new_index < step_size) {
                    self.index = @truncate(Log2StepSizeInt, new_index);
                    break;
                }
                self.block = try self.nextBlock();
            }
            // skip trailing '"'
            try self.incIndex();

            return self.string_buf.items;
        }

        /// assumes self.index is one past a leading '"'
        pub fn readStringAlloc(self: *Self, options: Options) ![]u8 {
            var list = std.ArrayListUnmanaged(u8){};
            errdefer {
                while (list.popOrNull()) |v|
                    parseFree(u8, v, options);
                list.deinit(options.allocator);
            }

            while (true) {
                const start = self.index;
                const new_index = @ctz(u64, self.block.string.quote >> self.index << self.index);
                try list.appendSlice(options.allocator, self.input_buf[start..new_index]);
                log("readStringAlloc start {} new_index {} string_buf '{s}'", .{ start, new_index, self.string_buf.items });
                if (new_index < step_size) {
                    self.index = @truncate(Log2StepSizeInt, new_index);
                    break;
                }
                self.block = try self.nextBlock();
            }
            // skip trailing '"'
            try self.incIndex();

            return list.toOwnedSlice(options.allocator);
        }

        /// assumes self.index is one past a leading '"'
        pub fn skipString(self: *Self) !void {
            while (true) {
                const new_index = @ctz(u64, self.block.string.quote >> self.index << self.index);
                if (new_index < step_size) {
                    self.index = @truncate(Log2StepSizeInt, new_index);
                    break;
                }
                self.block = try self.nextBlock();
            }
            // skip trailing '"'
            try self.incIndex();
        }

        pub fn readInt(self: *Self, comptime T: type) !T {
            try self.skipWs();
            const tok = try self.nextToken(self.token_buf.len);
            return try std.fmt.parseInt(T, tok, 10);
        }

        pub fn readFloat(self: *Self, comptime T: type) !T {
            try self.skipWs();
            const tok = try self.nextToken(self.token_buf.len);
            return try std.fmt.parseFloat(T, tok);
        }

        /// assumes self.index is one past '['
        pub fn readArrayAlloc(self: *Self, comptime T: type, options: Options) ![]T {
            var list = std.ArrayListUnmanaged(T){};
            errdefer list.deinit(options.allocator);
            while (true) {
                try list.append(options.allocator, try self.parseImpl(T, options));
                const next_char = self.nextChar() catch |err| switch (err) {
                    error.EndOfStream => return error.UnterminatedArray,
                    else => return err,
                };
                switch (next_char) {
                    ',' => {},
                    ']' => break,
                    else => return error.InvalidArray,
                }
            }
            return list.toOwnedSlice(options.allocator);
        }

        inline fn find_escaped(self: *Self, backslash_: u64) u64 {
            // If there was overflow, pretend the first character isn't a backslash
            var backslash = backslash_ & ~self.prev_escaped;
            const follows_escape = backslash << 1 | self.prev_escaped;

            // Get sequences starting on even bits by clearing out the odd series using +
            const even_bits: u64 = 0x5555555555555555;
            const odd_sequence_starts = backslash & ~even_bits & ~follows_escape;
            var sequences_starting_on_even_bits: u64 = undefined;
            // println("{b:0>64} | prev_escaped a", .{@bitReverse(u64, self.prev_escaped)});
            self.prev_escaped = @boolToInt(@addWithOverflow(u64, odd_sequence_starts, backslash, &sequences_starting_on_even_bits));
            // println("{b:0>64} | prev_escaped b", .{@bitReverse(u64, self.prev_escaped)});
            const invert_mask = sequences_starting_on_even_bits << 1; // The mask we want to return is the *escaped* bits, not escapes.

            // Mask every other backslashed character as an escaped character
            // Flip the mask for sequences that start on even bits, to correct them
            return (even_bits ^ invert_mask) & follows_escape;
        }

        fn nextStructuralIndex(self: *Self) u7 {
            return @ctz(u64, self.block.characters.op);
        }
        pub fn skipValue(self: *Self) !void {
            const Container = enum { arr, obj };
            var containers: [256]Container = undefined;
            var depth: u8 = 0;

            while (true) {
                const c = try self.nextChar();
                log("skipValue '{c}' depth {}", .{ c, depth });
                switch (c) {
                    '{' => {
                        if (depth >= containers.len) return error.MaxDepthExceeded;
                        containers[depth] = .obj;
                        depth += 1;
                    },
                    '[' => {
                        if (depth >= containers.len) return error.MaxDepthExceeded;
                        containers[depth] = .arr;
                        depth += 1;
                    },
                    '"' => try self.skipString(),
                    ':' => {},
                    ',' => if (depth == 0) break,
                    '}' => {
                        if (depth == 0) {
                            self.last_char = c;
                            break;
                        }
                        depth -= 1;
                        if (containers[depth] != .obj) return error.UnexpectedObjectEnd;
                    },
                    ']' => {
                        if (depth == 0) break;
                        depth -= 1;
                        if (containers[depth] != .arr) return error.UnexpectedArrayEnd;
                    },
                    else => {
                        std.log.err("skipValue unexpected character '{c}' depth {}\n", .{ c, depth });
                        return error.Unexpected;
                    },
                }
            }
        }

        pub fn dump(self: Self) void {
            log("", .{});
            var buf_copy = [1]u8{0x20} ** step_size;
            for (buf_copy) |*c, i| {
                if (mem.indexOfScalar(u8, &std.ascii.spaces, self.input_buf[i]) == null)
                    c.* = self.input_buf[i];
            }
            self.block.dump();
            log(("0123456789" ** 7)[0..step_size], .{});
            log("{s}: input_buf", .{&buf_copy});
        }

        fn parseImpl(self: *Self, comptime T: type, options: Options) !T {
            log("-- parseImpl {s}", .{@typeName(T)});
            defer log("-- done parseImpl {s}", .{@typeName(T)});

            const tinfo = @typeInfo(T);
            switch (tinfo) {
                .Struct => |structInfo| {
                    try self.expectChar('{');
                    var result: T = undefined;
                    var fields_seen = [_]bool{false} ** structInfo.fields.len;
                    errdefer {
                        inline for (structInfo.fields) |field, i| {
                            if (fields_seen[i] and !field.is_comptime)
                                parseFree(field.field_type, @field(result, field.name), options);
                        }
                    }

                    while (true) {
                        const next_char = self.nextChar() catch |err| switch (err) {
                            error.EndOfStream => break,
                            else => return err,
                        };
                        log("parseImpl next_char '{c}'", .{next_char});
                        switch (next_char) {
                            '"' => {
                                const key = self.readString() catch |err| switch (err) {
                                    error.EndOfStream => break,
                                    else => return err,
                                };
                                log("key {s}", .{key});
                                try self.expectChar(':');
                                inline for (structInfo.fields) |field, i| {
                                    if (std.mem.eql(u8, key, field.name)) {
                                        log("found key {s}", .{key});
                                        if (field.is_comptime) {
                                            const val = try self.parseImpl(field.field_type, options);
                                            if (!std.meta.eql(val, field.default_value.?))
                                                return error.UnexpectedValue;
                                        } else {
                                            @field(result, field.name) =
                                                if (@hasDecl(T, "on_" ++ field.name))
                                                try @field(T, "on_" ++ field.name)(options.allocator, self)
                                            else
                                                try self.parseImpl(field.field_type, options);
                                        }
                                        fields_seen[i] = true;
                                        break;
                                    }
                                } else {
                                    if (options.skip_unknown_fields) {
                                        try self.skipValue();
                                        continue;
                                    } else return error.UnknownField;
                                }
                            },
                            ',' => {}, // TODO: don't allow trailing comma
                            ']' => {
                                self.last_char = next_char;
                                return result;
                            },
                            '}' => {
                                self.last_char = next_char;
                                break;
                            },
                            else => return error.InvalidCharacter,
                        }
                    } // end while(true)
                    try self.expectChar('}');
                    inline for (structInfo.fields) |field, i| {
                        if (!fields_seen[i]) {
                            if (field.default_value) |default| {
                                if (!field.is_comptime)
                                    @field(result, field.name) = default;
                            } else return error.MissingField;
                        }
                    }
                    return result;
                },
                .Pointer => |ptrInfo| switch (ptrInfo.size) {
                    .Slice => {
                        const next_char = try self.nextChar();
                        switch (next_char) {
                            '"' => {
                                if (ptrInfo.child == u8) {
                                    return try self.readStringAlloc(options);
                                } else return error.UnsupportedStringType;
                            },
                            '[' => {
                                const result = try self.readArrayAlloc(ptrInfo.child, options);
                                return result;
                            },
                            else => return error.InvalidToken,
                        }
                    },
                    .One => {
                        var result = try options.allocator.create(ptrInfo.child);
                        errdefer options.allocator.destroy(result);
                        result.* = try self.parseImpl(ptrInfo.child, options);
                        return result;
                    },
                    else => @compileError(comptime std.fmt.comptimePrint("TODO Pointer size: {s}", .{@tagName(tinfo.Pointer.size)})),
                },
                .Int, .ComptimeInt => return self.readInt(T),
                .Float, .ComptimeFloat => return self.readFloat(T),
                .Bool => {
                    try self.skipWs();
                    const buf = try self.nextToken(4);

                    log(".Bool '{s}' next '{c}' ", .{ buf, self.input_buf[self.index -| 1] });
                    const i = std.mem.readIntSliceLittle(u32, buf);
                    return switch (i) {
                        std.mem.readIntLittle(u32, "true") => true,
                        std.mem.readIntLittle(u32, "fals") => if (self.input_buf[self.index -| 1] == 'e') blk: {
                            self.index += 1;
                            break :blk false;
                        } else error.InvalidBool,
                        else => error.InvalidBool,
                    };
                },
                .Optional => |optInfo| {
                    try self.skipWs();
                    const next_char = self.input_buf[self.index];
                    return if (next_char == 'n') blk: {
                        const tok = try self.nextToken(4);
                        const i = std.mem.readIntLittle(u32, tok[0..4]);
                        // log("buf '{s}' i {} - {}", .{ &buf, i, std.mem.readIntLittle(u24, "ull") });
                        break :blk if (i == std.mem.readIntLittle(u32, "null")) @as(T, null) else error.InvalidOptional;
                    } else blk: {
                        break :blk try self.parseImpl(optInfo.child, options);
                    };
                },
                .Enum => |enumInfo| {
                    try self.skipWs();
                    switch (self.input_buf[self.index]) {
                        '0'...'9', '-' => {
                            const tok = try self.nextToken(self.token_buf.len);
                            const n = try std.fmt.parseInt(enumInfo.tag_type, tok, 10);
                            return try std.meta.intToEnum(T, n);
                        },
                        '"' => {
                            try self.incIndex();
                            const str = try self.readString();
                            return std.meta.stringToEnum(T, str) orelse return error.InvalidEnumTag;
                        },
                        else => return error.InvalidEnumToken,
                    }
                },
                .Array => |arrInfo| {
                    try self.skipWs();
                    switch (self.input_buf[self.index]) {
                        '[' => {
                            try self.incIndex();
                            var result: T = undefined;
                            var i: usize = 0;
                            errdefer {
                                if (result.len > 0) while (true) : (i -= 1) {
                                    parseFree(arrInfo.child, result[i], options);
                                    if (i == 0) break;
                                };
                            }
                            while (i < result.len) : (i += 1) {
                                if (i > 0) try self.expectChar(',');
                                result[i] = try self.parseImpl(arrInfo.child, options);
                            }
                            const char = self.nextChar() catch return error.UnexpectedEndOfJson;
                            switch (char) {
                                ']' => {},
                                else => return error.UnexpectedToken,
                            }
                            return result;
                        },
                        '"' => {
                            if (arrInfo.child != u8) return error.UnexpectedToken;
                            try self.incIndex();
                            var result: T = undefined;
                            const str = try self.readString();
                            mem.copy(u8, &result, str);
                            return result;
                        },
                        else => return error.UnexpectedToken,
                    }
                    unreachable;
                },
                .Union => |unionInfo| {
                    try self.expectChar('{');
                    try self.expectChar('"');
                    const key = self.readString() catch |err| switch (err) {
                        error.EndOfStream => return error.InvalidUnionObject,
                        else => return err,
                    };
                    log("union key {s}", .{key});
                    try self.expectChar(':');
                    inline for (unionInfo.fields) |field| {
                        if (std.mem.eql(u8, key, field.name)) {
                            var result = @unionInit(T, field.name, try self.parseImpl(field.field_type, options));
                            errdefer parseFree(T, result, options);
                            try self.expectChar('}');
                            return result;
                        }
                    }
                    return error.NoUnionMembersMatched;
                },
                else => @compileError(comptime std.fmt.comptimePrint("TODO: {s}", .{@tagName(tinfo)})),
            }
        }
    };
}

extern fn @"llvm.x86.avx2.pshuf.b"(a: u8x32, b: u8x32) u8x32;
inline fn shuffleEpi8(a: u8x32, b: u8x32) u8x32 {
    return @"llvm.x86.avx2.pshuf.b"(a, b);
}

const CharacterBlock = struct {
    whitespace: u64,
    op: u64,
    pub fn classify(input_vec: u8x64) CharacterBlock {
        // These lookups rely on the fact that anything < 127 will match the lower 4 bits, which is why
        // we can't use the generic lookup_16.
        const whitespace_table: u8x32 = [16]u8{
            ' ', 100,  100,  100, 17,  100,  113, 2,
            100, '\t', '\n', 112, 100, '\r', 100, 100,
        } ** 2;

        // The 6 operators (:,[]{}) have these values:
        //
        // , 2C
        // : 3A
        // [ 5B
        // { 7B
        // ] 5D
        // } 7D
        //
        // If you use | 0x20 to turn [ and ] into { and }, the lower 4 bits of each character is unique.
        // We exploit this, using a simd 4-bit lookup to tell us which character match against, and then
        // match it (against | 0x20).
        //
        // To prevent recognizing other characters, everything else gets compared with 0, which cannot
        // match due to the | 0x20.
        //
        // NOTE: Due to the | 0x20, this ALSO treats <FF> and <SUB> (control characters 0C and 1A) like ,
        // and :. This gets caught in stage 2, which checks the actual character to ensure the right
        // operators are in the right places.
        const op_table: u8x32 = [16]u8{
            0, 0, 0, 0,
            0, 0, 0, 0,
            0,   0,   ':', '{', // : = 3A, [ = 5B, { = 7B
            ',', '}', 0,
            0, // , = 2C, ] = 5D, } = 7D
        } ** 2;

        // We compute whitespace and op separately. If later code only uses one or the
        // other, given the fact that all functions are aggressively inlined, we can
        // hope that useless computations will be omitted. This is namely case when
        // minifying (we only need whitespace).

        const in = @bitCast([64]u8, input_vec);
        const chunk0: u8x32 = in[0..32].*;
        const chunk1: u8x32 = in[32..64].*;
        const wss: [2]u8x32 = .{
            shuffleEpi8(whitespace_table, chunk0),
            shuffleEpi8(whitespace_table, chunk1),
        };
        const whitespace = input_vec == @bitCast(u8x64, wss);
        // Turn [ and ] into { and }
        const curlified = input_vec | @splat(64, @as(u8, 0x20));
        const ops: [2]u8x32 = .{
            shuffleEpi8(op_table, chunk0),
            shuffleEpi8(op_table, chunk1),
        };
        const op = curlified == @bitCast(u8x64, ops);

        return .{ .whitespace = @ptrCast(*const u64, &whitespace).*, .op = @ptrCast(*const u64, &op).* };
    }

    pub inline fn scalar(cb: CharacterBlock) u64 {
        return ~(cb.op | cb.whitespace);
    }

    fn dump(cb: CharacterBlock) void {
        log("CharacterBlock ", .{});
        log("{b:0>64}: whitespace", .{@bitReverse(u64, cb.whitespace)});
        log("{b:0>64}: op", .{@bitReverse(u64, cb.op)});
    }
};

const Block = struct {
    string: StringBlock,
    characters: CharacterBlock,
    follows_nonquote_scalar: u64,

    inline fn structural_start(block: Block) u64 {
        return block.potential_structural_start() & ~block.string.string_tail();
    }
    inline fn potential_structural_start(block: Block) u64 {
        return block.characters.op | block.potential_scalar_start();
    }
    inline fn potential_scalar_start(block: Block) u64 {
        // The term "scalar" refers to anything except structural characters and white space
        // (so letters, numbers, quotes).
        // Whenever it is preceded by something that is not a structural element ({,},[,],:, ") nor a white-space
        // then we know that it is irrelevant structurally.
        return block.characters.scalar() & ~block.follows_nonquote_scalar;
    }
    inline fn non_quote_inside_string(block: Block, mask: u64) u64 {
        return block.string.non_quote_inside_string(mask);
    }

    fn dump(block: Block) void {
        block.string.dump();
        block.characters.dump();
        log("{b:0>64}: follows_nonquote_scalar", .{@bitReverse(u64, block.follows_nonquote_scalar)});
    }
};

// pub const log_level = .debug;
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
pub fn main() !void {
    const file_name = if (std.os.argv.len > 1) mem.span(std.os.argv[1]) else {
        std.log.err("\nusage <exe> <json_filename>\n", .{});
        return std.os.exit(1);
    };
    const f = try std.fs.cwd().openFile(file_name, .{ .read = true });
    defer f.close();

    // const Status = struct {
    //     id: usize,
    // };
    // const Twitter = struct {
    //     statuses: []const Status,
    // };

    // const allocator = &gpa.allocator;
    // defer _ = gpa.deinit();
    // const allocator = &arena.allocator;
    const allocator = std.heap.c_allocator;

    var res = try parse([]const struct { a: u8 }, f.reader(), .{
        .allocator = allocator,
        .skip_unknown_fields = true,
    });
    _ = res;
    // try testing.expectEqual(@as(usize, 100), res.statuses.len);
    // std.debug.print("res.statuses.len {} last status {}\n", .{ res.statuses.len, res.statuses[res.statuses.len - 1] });
    // allocator.free(res.statuses);
    // try testing.expectEqual(@as(usize, 505874924095815681), res.statuses[0].id);
}
