const std = @import("std");
const mem = std.mem;
const log = std.log.debug;

const u8x64 = std.meta.Vector(64, u8);
const u8x32 = std.meta.Vector(32, u8);
const u64x2 = std.meta.Vector(2, u64);
const step_size = 64;
const I = std.meta.Int(.unsigned, std.math.log2_int(u8, step_size));
const debug = true;

pub const Options = struct {
    /// allocator to use when pointer or slice fields are found
    allocator: *mem.Allocator,

    /// if false, unknown fields will cause an error.UnknownField
    /// otherwise they will be skipped. defaults to true.
    skip_unknown_fields: bool = true,
};

/// if T includes methods such as `on_field` 
/// with the following signature, they will be called when `field` is found: 
///   `pub fn on_field(reader: anytype, allocator: *mem.Allocator, p: *Parser) !Ret`.
/// The return type Ret must match the type of `field`.
pub fn parse(comptime T: type, reader: anytype, options: Options) !T {
    var buffered_reader = std.io.bufferedReader(reader);
    var parser = try Parser.init(options.allocator, &buffered_reader);
    defer parser.deinit();
    return parseImpl(T, &parser, &buffered_reader, options);
}

fn parseImpl(comptime T: type, parser: *Parser, reader: anytype, options: Options) !T {
    log("-- parseImpl {s}", .{@typeName(T)});
    defer log("-- done parseImpl {s}", .{@typeName(T)});
    const R = @TypeOf(reader);
    if (@typeInfo(R) != .Pointer)
        @compileError(comptime std.fmt.comptimePrint("reader must be a pointer. found type '{s}'", .{@typeName(R)}));
    const tinfo = @typeInfo(T);
    switch (tinfo) {
        .Struct => |structInfo| {
            try parser.expectChar('{', reader);
            var result: T = undefined;
            var fields_seen = [_]bool{false} ** structInfo.fields.len;
            errdefer {
                inline for (structInfo.fields) |field, i| {
                    if (fields_seen[i] and !field.is_comptime)
                        parseFree(field.field_type, @field(result, field.name), options);
                }
            }

            while (true) {
                const next_char = parser.nextChar(reader) catch |err| switch (err) {
                    error.EndOfStream => break,
                    else => return err,
                };
                log("parseImpl next_char '{c}'", .{next_char});
                switch (next_char) {
                    '"' => {
                        const key = parser.readString(reader) catch |err| switch (err) {
                            error.EndOfStream => break,
                            else => return err,
                        };
                        log("key {s}", .{key});
                        try parser.expectChar(':', reader);
                        inline for (structInfo.fields) |field, i| {
                            if (std.mem.eql(u8, key, field.name)) {
                                log("found key {s}", .{key});
                                if (field.is_comptime) {
                                    const val = try parseImpl(field.field_type, parser, reader, options);
                                    if (!std.meta.eql(val, field.default_value.?))
                                        return error.UnexpectedValue;
                                } else {
                                    @field(result, field.name) =
                                        if (@hasDecl(T, "on_" ++ field.name))
                                        try @field(T, "on_" ++ field.name)(reader, options.allocator, parser)
                                    else
                                        try parseImpl(field.field_type, parser, reader, options);
                                }
                                fields_seen[i] = true;
                                break;
                            }
                        } else {
                            if (options.skip_unknown_fields) {
                                try parser.skipValue(reader);
                                continue;
                            } else return error.UnknownField;
                        }
                    },
                    ',' => {}, // TODO: don't allow trailing comma
                    ']' => {
                        parser.last_char = next_char;
                        return result;
                    },
                    '}' => {
                        parser.last_char = next_char;
                        break;
                    },
                    else => return error.InvalidCharacter,
                }
            } // end while(true)
            try parser.expectChar('}', reader);
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
                const next_char = try parser.nextChar(reader);
                switch (next_char) {
                    '"' => {
                        if (ptrInfo.child == u8) {
                            return try parser.readStringAlloc(reader, options);
                        } else return error.UnsupportedStringType;
                    },
                    '[' => {
                        const result = try parser.readArrayAlloc(ptrInfo.child, reader, options);
                        return result;
                    },
                    else => return error.InvalidToken,
                }
            },
            .One => {
                var result = try options.allocator.create(ptrInfo.child);
                errdefer options.allocator.destroy(result);
                result.* = try parseImpl(ptrInfo.child, parser, reader, options);
                return result;
            },
            else => @compileError(comptime std.fmt.comptimePrint("TODO Pointer size: {s}", .{@tagName(tinfo.Pointer.size)})),
        },
        .Int, .ComptimeInt => return parser.readInt(T, reader),
        .Float, .ComptimeFloat => return parser.readFloat(T, reader),
        .Bool => {
            try parser.skipWs(reader);
            const buf = try parser.nextToken(reader, 4);

            log(".Bool '{s}' next '{c}' ", .{ buf, parser.input_buf[parser.index -| 1] });
            const i = std.mem.readIntSliceLittle(u32, buf);
            return switch (i) {
                std.mem.readIntLittle(u32, "true") => true,
                std.mem.readIntLittle(u32, "fals") => if (parser.input_buf[parser.index -| 1] == 'e') blk: {
                    parser.index += 1;
                    break :blk false;
                } else error.InvalidBool,
                else => error.InvalidBool,
            };
        },
        .Optional => |optInfo| {
            try parser.skipWs(reader);
            const next_char = parser.input_buf[parser.index];
            return if (next_char == 'n') blk: {
                const tok = try parser.nextToken(reader, 4);
                const i = std.mem.readIntLittle(u32, tok[0..4]);
                // log("buf '{s}' i {} - {}", .{ &buf, i, std.mem.readIntLittle(u24, "ull") });
                break :blk if (i == std.mem.readIntLittle(u32, "null")) @as(T, null) else error.InvalidOptional;
            } else blk: {
                break :blk try parseImpl(optInfo.child, parser, reader, options);
            };
        },
        .Enum => |enumInfo| {
            try parser.skipWs(reader);
            switch (parser.input_buf[parser.index]) {
                '0'...'9', '-' => {
                    const tok = try parser.nextToken(reader, parser.token_buf.len);
                    const n = try std.fmt.parseInt(enumInfo.tag_type, tok, 10);
                    return try std.meta.intToEnum(T, n);
                },
                '"' => {
                    try parser.incIndex(reader);
                    const str = try parser.readString(reader);
                    return std.meta.stringToEnum(T, str) orelse return error.InvalidEnumTag;
                },
                else => return error.InvalidEnumToken,
            }
        },
        .Array => |arrInfo| {
            try parser.skipWs(reader);
            switch (parser.input_buf[parser.index]) {
                '[' => {
                    try parser.incIndex(reader);
                    var result: T = undefined;
                    var i: usize = 0;
                    errdefer {
                        if (result.len > 0) while (true) : (i -= 1) {
                            parseFree(arrInfo.child, result[i], options);
                            if (i == 0) break;
                        };
                    }
                    while (i < result.len) : (i += 1) {
                        if (i > 0) try parser.expectChar(',', reader);
                        result[i] = try parseImpl(arrInfo.child, parser, reader, options);
                    }
                    const char = parser.nextChar(reader) catch return error.UnexpectedEndOfJson;
                    switch (char) {
                        ']' => {},
                        else => return error.UnexpectedToken,
                    }
                    return result;
                },
                '"' => {
                    if (arrInfo.child != u8) return error.UnexpectedToken;
                    try parser.incIndex(reader);
                    var result: T = undefined;
                    const str = try parser.readString(reader);
                    mem.copy(u8, &result, str);
                    return result;
                },
                else => return error.UnexpectedToken,
            }
            unreachable;
        },
        .Union => |unionInfo| {
            try parser.expectChar('{', reader);
            try parser.expectChar('"', reader);
            const key = parser.readString(reader) catch |err| switch (err) {
                error.EndOfStream => return error.InvalidUnionObject,
                else => return err,
            };
            log("union key {s}", .{key});
            try parser.expectChar(':', reader);
            inline for (unionInfo.fields) |field| {
                if (std.mem.eql(u8, key, field.name)) {
                    var result = @unionInit(T, field.name, try parseImpl(field.field_type, parser, reader, options));
                    errdefer parseFree(T, result, options);
                    try parser.expectChar('}', reader);
                    return result;
                }
            }
            return error.NoUnionMembersMatched;
        },
        else => @compileError(comptime std.fmt.comptimePrint("TODO: {s}", .{@tagName(tinfo)})),
    }
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
            } else unreachable;
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
pub inline fn carrylessMul(a: u64x2, b: u64x2) u64x2 {
    return @bitCast(
        u64x2,
        @"llvm.x86.pclmulqdq"(a, b, 0),
    );
}

pub const Parser = struct {
    allocator: *mem.Allocator,
    prev_escaped: u64 = 0,
    prev_scalar: u64 = 0,
    prev_in_string: u64 = 0,
    block: Block,
    /// the current index within input_buf
    index: I = 0,
    input_buf: [step_size]u8 = undefined,
    /// temporary buffer for keys and string values
    string_buf: std.ArrayListUnmanaged(u8) = .{},
    /// temporary buffer for numbers, true, false and null tokens
    token_buf: [40]u8 = undefined,
    /// single character lookbehind
    last_char: ?u8 = null,

    pub fn init(allocator: *mem.Allocator, reader: anytype) !Parser {
        var p: Parser = .{
            .allocator = allocator,
            .block = undefined,
        };
        p.block = try p.nextBlock(reader);
        return p;
    }
    pub fn deinit(p: *Parser) void {
        p.string_buf.deinit(p.allocator);
    }

    inline fn nextStringBlock(parser: *Parser, input_vec: u8x64) StringBlock {
        const backslash_vec = input_vec == @splat(64, @as(u8, '\\'));
        const backslash = @bitCast(u64, backslash_vec);
        const escaped = parser.find_escaped(backslash);
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
        in_string ^= parser.prev_in_string;
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
        parser.prev_in_string = @bitCast(u64, @bitCast(i64, in_string) >> 63);

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

    fn nextBlock(parser: *Parser, reader: anytype) !Block {
        parser.input_buf = [1]u8{0x20} ** step_size;
        parser.index = 0;
        const n = try reader.read(&parser.input_buf);
        if (n == 0) return error.EndOfStream;
        const string = parser.nextStringBlock(parser.input_buf);
        // identifies the white-space and the structurat characters
        const characters = CharacterBlock.classify(parser.input_buf);

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
        const follows_nonquote_scalar = follows(nonquote_scalar, &parser.prev_scalar);
        // if (unescaped & strings.in_string != 0) return error.UnescapedCharacters;
        return Block{
            .string = string, // strings is a function-local object so either it moves or the copy is elided.
            .characters = characters,
            .follows_nonquote_scalar = follows_nonquote_scalar,
        };
    }

    fn nextChar(parser: *Parser, reader: anytype) !u8 {
        if (parser.last_char) |lc| {
            log("nextChar() using last_char '{c}'", .{lc});
            defer parser.last_char = null;
            return lc;
        }
        while (true) {
            log("nextChar() index {}", .{parser.index});
            // parser.dump();

            log("{b:0>64} : quote", .{@bitReverse(u64, parser.block.string.quote >> parser.index << parser.index)});
            const index = std.math.min(
                @ctz(u64, parser.block.string.quote >> parser.index << parser.index),
                @ctz(u64, parser.block.characters.op >> parser.index << parser.index),
            );
            log("nextChar() index2 {}", .{index});
            if (index < step_size) {
                const new_index = @truncate(I, index);
                const result = parser.input_buf[new_index];
                log("nextChar result '{c}'", .{result});
                if (new_index == step_size - 1) {
                    parser.block = try parser.nextBlock(reader);
                } else parser.index = new_index + 1;
                return result;
            }
            parser.block = try parser.nextBlock(reader);
        }
        unreachable;
    }

    fn nextToken(parser: *Parser, reader: anytype, comptime max_len: u32) ![]const u8 {
        log("nextToken index {}", .{parser.index});
        try parser.skipWs(reader);
        // log("nextToken after skipWs index {}", .{parser.index});
        // parser.dump();
        const scalar = (parser.block.characters.whitespace | parser.block.characters.op) >> parser.index;
        const token_len = if (scalar == 0) step_size - @as(u7, parser.index) else @ctz(u64, scalar);
        log("{b:0>64} : scalar, token_len {}", .{ @bitReverse(u64, scalar), token_len });
        const token_end = parser.index + std.math.min(token_len, max_len);
        // const token_end = parser.index + token_len;
        log("{b:0>64} : scalar, token_len {} token_end {}", .{ @bitReverse(u64, scalar), token_len, token_end });
        const slice = if (token_end < step_size) blk: {
            mem.copy(u8, &parser.token_buf, parser.input_buf[parser.index..token_end]);
            break :blk parser.token_buf[0..token_len];
        } else {
            log("nextToken index 2 {}", .{parser.index});
            mem.copy(u8, &parser.token_buf, parser.input_buf[parser.index..]);
            parser.block = try parser.nextBlock(reader);
            // parser.dump();
            const scalar2 = (parser.block.characters.whitespace | parser.block.characters.op);
            const token_len2 = @ctz(u64, scalar2);
            log("{b:0>64} : scalar2, token_len2 {}", .{ @bitReverse(u64, scalar2), token_len2 });
            const slice2 = if (token_len2 < step_size)
                parser.input_buf[0..token_len2]
            else
                return error.TokenTooLong;
            mem.copy(u8, parser.token_buf[token_len..], slice2);
            parser.index = @truncate(I, token_len2);
            return parser.token_buf[0..std.math.min(token_len + token_len2, max_len)];
        };
        log("nextToken slice '{s}'", .{slice});
        parser.index += @truncate(I, slice.len);
        return slice;
    }

    fn expectChar(parser: *Parser, comptime expected: u8, reader: anytype) !void {
        log("expectChar('{c}')", .{expected});
        const actual = try parser.nextChar(reader);
        if (expected != actual) {
            std.log.err("expecting '{c}' but found '{c}'", .{ expected, actual });
            return error.UnexpectedChar;
        }
    }

    fn skipWs(parser: *Parser, _: anytype) !void {
        const mask = @as(u64, 1) << parser.index;
        // log("{b:0>64}: mask", .{@bitReverse(u64, mask)});
        // log("{b:0>64}: parser.block.characters.whitespace", .{@bitReverse(u64, parser.block.characters.whitespace)});

        const is_next_ws = @boolToInt(mask & parser.block.characters.whitespace != 0);
        parser.index += is_next_ws * try std.math.cast(I, @ctz(u64, ~parser.block.characters.whitespace >> parser.index));
        // log("skipWs new index {}", .{parser.index});
    }

    fn incIndex(parser: *Parser, reader: anytype) !void {
        if (@addWithOverflow(I, parser.index, 1, &parser.index))
            parser.block = try parser.nextBlock(reader);
    }

    /// assumes parser.index is one past leading '"'
    fn readString(parser: *Parser, reader: anytype) ![]const u8 {
        parser.string_buf.items.len = 0;
        while (true) {
            const start = parser.index;
            const new_index = @ctz(u64, parser.block.string.quote >> parser.index << parser.index);
            try parser.string_buf.appendSlice(parser.allocator, parser.input_buf[start..new_index]);
            log("readString start {} new_index {} string_buf '{s}'", .{ start, new_index, parser.string_buf.items });
            if (new_index < step_size) {
                parser.index = @truncate(I, new_index);
                break;
            }
            parser.block = try parser.nextBlock(reader);
        }
        // skip trailing '"'
        try parser.incIndex(reader);

        return parser.string_buf.items;
    }

    /// assumes parser.index is one past a leading '"'
    fn readStringAlloc(parser: *Parser, reader: anytype, options: Options) ![]u8 {
        var list = std.ArrayListUnmanaged(u8){};
        errdefer {
            while (list.popOrNull()) |v|
                parseFree(u8, v, options);
            list.deinit(options.allocator);
        }

        while (true) {
            const start = parser.index;
            const new_index = @ctz(u64, parser.block.string.quote >> parser.index << parser.index);
            try list.appendSlice(options.allocator, parser.input_buf[start..new_index]);
            log("readStringAlloc start {} new_index {} string_buf '{s}'", .{ start, new_index, parser.string_buf.items });
            if (new_index < step_size) {
                parser.index = @truncate(I, new_index);
                break;
            }
            parser.block = try parser.nextBlock(reader);
        }
        // skip trailing '"'
        try parser.incIndex(reader);

        return list.toOwnedSlice(options.allocator);
    }

    /// assumes parser.index is one past a leading '"'
    fn skipString(parser: *Parser, reader: anytype) !void {
        while (true) {
            const new_index = @ctz(u64, parser.block.string.quote >> parser.index << parser.index);
            if (new_index < step_size) {
                parser.index = @truncate(I, new_index);
                break;
            }
            parser.block = try parser.nextBlock(reader);
        }
        // skip trailing '"'
        try parser.incIndex(reader);
    }

    fn readInt(parser: *Parser, comptime T: type, reader: anytype) !T {
        try parser.skipWs(reader);
        const tok = try parser.nextToken(reader, parser.token_buf.len);
        return try std.fmt.parseInt(T, tok, 10);
    }

    fn readFloat(parser: *Parser, comptime T: type, reader: anytype) !T {
        try parser.skipWs(reader);
        const tok = try parser.nextToken(reader, parser.token_buf.len);
        return try std.fmt.parseFloat(T, tok);
    }

    /// assumes parser.index is one past '['
    pub fn readArrayAlloc(parser: *Parser, comptime T: type, reader: anytype, options: Options) ![]T {
        var list = std.ArrayListUnmanaged(T){};
        errdefer list.deinit(options.allocator);
        while (true) {
            try list.append(options.allocator, try parseImpl(T, parser, reader, options));
            const next_char = parser.nextChar(reader) catch |err| switch (err) {
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

    inline fn find_escaped(parser: *Parser, backslash_: u64) u64 {
        // If there was overflow, pretend the first character isn't a backslash
        var backslash = backslash_ & ~parser.prev_escaped;
        const follows_escape = backslash << 1 | parser.prev_escaped;

        // Get sequences starting on even bits by clearing out the odd series using +
        const even_bits: u64 = 0x5555555555555555;
        const odd_sequence_starts = backslash & ~even_bits & ~follows_escape;
        var sequences_starting_on_even_bits: u64 = undefined;
        // println("{b:0>64} | prev_escaped a", .{@bitReverse(u64, parser.prev_escaped)});
        parser.prev_escaped = @boolToInt(@addWithOverflow(u64, odd_sequence_starts, backslash, &sequences_starting_on_even_bits));
        // println("{b:0>64} | prev_escaped b", .{@bitReverse(u64, parser.prev_escaped)});
        const invert_mask = sequences_starting_on_even_bits << 1; // The mask we want to return is the *escaped* bits, not escapes.

        // Mask every other backslashed character as an escaped character
        // Flip the mask for sequences that start on even bits, to correct them
        return (even_bits ^ invert_mask) & follows_escape;
    }

    fn nextStructuralIndex(parser: *Parser) u7 {
        return @ctz(u64, parser.block.characters.op);
    }
    fn skipValue(parser: *Parser, reader: anytype) !void {
        const Container = enum { arr, obj };
        var containers: [256]Container = undefined;
        var depth: u8 = 0;

        while (true) {
            const c = try parser.nextChar(reader);
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
                '"' => try parser.skipString(reader),
                ':' => {},
                ',' => if (depth == 0) break,
                '}' => {
                    if (depth == 0) {
                        parser.last_char = c;
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

    fn dump(parser: Parser) void {
        log("", .{});
        var buf_copy = [1]u8{0x20} ** step_size;
        for (buf_copy) |*c, i| {
            if (mem.indexOfScalar(u8, &std.ascii.spaces, parser.input_buf[i]) == null)
                c.* = parser.input_buf[i];
        }
        parser.block.dump();
        log(("0123456789" ** 7)[0..step_size], .{});
        log("{s}: input_buf", .{&buf_copy});
    }
};

extern fn @"llvm.x86.avx2.pshuf.b"(a: u8x32, b: u8x32) u8x32;
pub inline fn shuffleEpi8(a: u8x32, b: u8x32) u8x32 {
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

const testing = std.testing;

const Expected = union(enum) {
    char: u8,
    string: []const u8,
    token: []const u8,
};

fn expectNext(p: *Parser, reader: anytype, expected: Expected) !void {
    switch (expected) {
        .char => {
            const c = try p.nextChar(reader);
            try testing.expectEqual(expected.char, c);
        },
        .string => {
            const s = try p.readString(reader);
            try testing.expectEqualStrings(expected.string, s);
        },
        .token => {
            const s = try p.nextToken(reader, std.math.maxInt(u32));
            try testing.expectEqualStrings(expected.token, s);
        },
    }
}

test "nexts" {
    // testing.log_level = .debug;
    const text =
        \\  {"asdf" : false,"bsdf":null }
    ;
    var fbs = std.io.fixedBufferStream(text);
    var reader = fbs.reader();
    var p = try Parser.init(std.heap.page_allocator, reader);
    try expectNext(&p, reader, .{ .char = '{' });
    try expectNext(&p, reader, .{ .char = '"' });
    try expectNext(&p, reader, .{ .string = "asdf" });
    try expectNext(&p, reader, .{ .char = ':' });
    try expectNext(&p, reader, .{ .token = "false" });
    try expectNext(&p, reader, .{ .char = ',' });
    try expectNext(&p, reader, .{ .char = '"' });
    try expectNext(&p, reader, .{ .string = "bsdf" });
    try expectNext(&p, reader, .{ .char = ':' });
    try expectNext(&p, reader, .{ .token = "null" });
    try expectNext(&p, reader, .{ .char = '}' });
}

test "span multiple blocks" {
    // testing.log_level = .debug;
    const input =
        \\{                 "l":[10000,200000,3000000,18446744073709551615]}
    ;
    var p: Parser = undefined;
    try testing.expectEqual(@as(usize, 64), p.input_buf.len);
    try testing.expectEqual(@as(usize, 66), input.len);
    try testing.expectEqual(@as(u8, '5'), input[63]);
    var fbs = std.io.fixedBufferStream(input);
    const T = struct { l: []const usize };
    const result = try parse(T, fbs.reader(), .{ .allocator = std.heap.page_allocator });
    try testing.expectEqual(@as(usize, 4), result.l.len);
    try testing.expectEqual(@as(usize, 18446744073709551615), result.l[3]);
}

test "twitter.json" {
    // testing.log_level = .debug;
    const f = try std.fs.cwd().openFile("testdata/twitter.json", .{ .read = true });
    defer f.close();

    const Status = struct {
        id: usize,
    };
    const Twitter = struct {
        statuses: []const Status,
    };

    const res = try parse(Twitter, f.reader(), .{
        .allocator = std.heap.page_allocator,
        .skip_unknown_fields = true,
    });
    try testing.expectEqual(@as(usize, 1), res.statuses.len);
    try testing.expectEqual(@as(usize, 505874924095815681), res.statuses[0].id);
}

test "basic" {
    @setEvalBranchQuota(3000);
    // testing.log_level = .debug;
    const E = enum { a, b };
    const T = struct {
        num: u8,
        str: []const u8,
        bool1: bool,
        bool2: bool,
        opt_null: ?u8,
        opt_42: ?u8,
        opt_neg42: ?i8,
        opt_float422: ?f32,
        float422: f32,
        slice_arr: []u8,
        nested_struct: struct { a: u8 },
        slice_nonu8: []usize,
        custom: u8, // uses on_custom method
        num_large: u128,
        float_large: f64,
        ptr_one: *u8,
        enum1: E,
        enum2: E,
        arr1: [2]u8,
        arr2: [2]u8,
        union_bare: union { a: usize, b: []const u8 },
        union_enum: union(enum) { a: usize, b: []const u8 },
        default_missing: u8 = 66,

        pub fn on_custom(reader: anytype, _: *mem.Allocator, p: *Parser) !u8 {
            _ = reader;
            _ = try p.nextToken(reader, p.token_buf.len);
            return 55;
        }
    };
    const input =
        \\ { "num" : 42 ,
        \\ "str" : "string",
        \\ "bool1" : true,
        \\ "bool2":false,
        \\ "opt_null": null,
        \\ "opt_42":42,
        \\ "opt_neg42":-42,
        \\ "opt_float422":42.2,
        \\ "float422":42.2,
        \\ "slice_arr": [ 1 , 2 , 3 ] , 
        \\ "nested_struct":{"a":42},
        \\ "slice_nonu8":[10000,200000,3000000,18446744073709551615],
        \\ "num_large": 340282366920938463463374607431768211455,
        \\ "float_large": 1.7976931348623157e+308,
        \\"custom":66,
        \\ "ptr_one": 101,
        \\ "enum1": 0,
        \\ "enum2": "b",
        \\ "arr1": [1,2],
        \\ "arr2": "ab",
        \\ "union_bare": {"a": 42},
        \\ "union_enum": {"b": "bstr"},
        \\}
    ;
    var fbs = std.io.fixedBufferStream(input);
    const result = try parse(T, fbs.reader(), .{
        .allocator = std.heap.page_allocator,
    });
    try testing.expectEqual(@as(u8, 42), result.num);
    try testing.expectEqualStrings("string", result.str);
    try testing.expect(result.bool1);
    try testing.expect(!result.bool2);
    try testing.expect(result.opt_null == null);
    try testing.expectEqual(@as(?u8, 42), result.opt_42);
    try testing.expectEqual(@as(?i8, -42), result.opt_neg42);
    try testing.expect(result.opt_float422 != null);
    try testing.expectApproxEqAbs(@as(f32, 42.2), result.opt_float422.?, std.math.epsilon(f32));
    try testing.expectApproxEqAbs(@as(f32, 42.2), result.float422, std.math.epsilon(f32));
    try testing.expectEqualSlices(u8, &[_]u8{ 1, 2, 3 }, result.slice_arr);
    try testing.expectEqual(@as(u8, 42), result.nested_struct.a);
    try testing.expectEqualSlices(usize, &[_]usize{ 10_000, 200_000, 3_000_000, std.math.maxInt(usize) }, result.slice_nonu8);
    try testing.expectEqual(@as(u8, 55), result.custom);
    try testing.expectEqual(@as(u128, std.math.maxInt(u128)), result.num_large);
    try testing.expectApproxEqAbs(@as(f64, std.math.f64_max), result.float_large, std.math.epsilon(f64));
    try testing.expectEqual(E.a, result.enum1);
    try testing.expectEqual(E.b, result.enum2);
    try testing.expectEqual([_]u8{ 1, 2 }, result.arr1);
    try testing.expectEqualStrings("ab", &result.arr2);
    try testing.expectEqual(@as(usize, 42), result.union_bare.a);
    try testing.expect(result.union_enum == .b);
    try testing.expectEqualStrings("bstr", result.union_enum.b);
    try testing.expectEqual(@as(u8, 66), result.default_missing);
}

test "unknown field" {
    const input =
        \\{"a":1}
    ;
    const T = struct { b: u8 };
    var fbs = std.io.fixedBufferStream(input);
    try testing.expectError(error.UnknownField, parse(T, fbs.reader(), .{
        .allocator = std.heap.page_allocator,
        .skip_unknown_fields = false,
    }));
}

test "comptime fields" {
    {
        const input =
            \\{"a":1}
        ;
        const T = struct {
            comptime a: u8 = 44,
        };
        var fbs = std.io.fixedBufferStream(input);
        try testing.expectError(error.UnexpectedValue, parse(T, fbs.reader(), .{
            .allocator = std.heap.page_allocator,
            .skip_unknown_fields = false,
        }));
    }
    {
        const input =
            \\{"b": 2.2}
        ;
        const T = struct {
            comptime b: f32 = 1.1,
        };
        var fbs = std.io.fixedBufferStream(input);
        try testing.expectError(error.UnexpectedValue, parse(T, fbs.reader(), .{
            .allocator = std.heap.page_allocator,
            .skip_unknown_fields = false,
        }));
    }
    {
        const input =
            \\{"a": 44, "b": 1.1}
        ;
        const T = struct {
            comptime a: u8 = 44,
            comptime b: f32 = 1.1,
        };
        var fbs = std.io.fixedBufferStream(input);
        _ = try parse(T, fbs.reader(), .{
            .allocator = std.heap.page_allocator,
            .skip_unknown_fields = false,
        });
        // don't need to verify field values, just make sure the fields are found
    }
}

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
