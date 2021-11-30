const std = @import("std");
const mem = std.mem;
const json = @import("simdjson.zig");
const Options = json.Options;
const testing = std.testing;

const Expected = union(enum) {
    char: u8,
    string: []const u8,
    token: []const u8,
};

fn expectNext(p: anytype, expected: Expected) !void {
    switch (expected) {
        .char => {
            const c = try p.nextChar();
            try testing.expectEqual(expected.char, c);
        },
        .string => {
            const s = try p.readString();
            try testing.expectEqualStrings(expected.string, s);
        },
        .token => {
            const s = try p.nextToken(std.math.maxInt(u32));
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
    var p = try json.parser(testing.allocator, &fbs.reader());
    defer p.deinit();
    try expectNext(&p, .{ .char = '{' });
    try expectNext(&p, .{ .char = '"' });
    try expectNext(&p, .{ .string = "asdf" });
    try expectNext(&p, .{ .char = ':' });
    try expectNext(&p, .{ .token = "false" });
    try expectNext(&p, .{ .char = ',' });
    try expectNext(&p, .{ .char = '"' });
    try expectNext(&p, .{ .string = "bsdf" });
    try expectNext(&p, .{ .char = ':' });
    try expectNext(&p, .{ .token = "null" });
    try expectNext(&p, .{ .char = '}' });
}

test "span multiple blocks" {
    // testing.log_level = .debug;
    const input =
        //0         1         2         3         4         5         6
        //0123456789012345678901234567890123456789012345678901234567890123
        \\{                 "l":[100000,200000,3000000,18446744073709551615]}
        //                                                               ^ block ends here
    ;
    var fbs = std.io.fixedBufferStream(input);
    var p = try json.parser(testing.allocator, &fbs.reader());
    try testing.expectEqual(@as(usize, 64), p.input_buf.len);
    try testing.expectEqual(@as(usize, 67), input.len);
    try testing.expectEqual(@as(u8, '1'), input[63]);
    const T = struct { l: []const usize };
    const options: Options = .{ .allocator = testing.allocator };
    const result = try json.parseText(T, input, options);
    defer json.parseFree(T, result, options);
    try testing.expectEqual(@as(usize, 4), result.l.len);
    try testing.expectEqual(@as(usize, 18446744073709551615), result.l[3]);
}

test "twitter.json" {
    // testing.log_level = .debug;
    const f = try std.fs.cwd().openFile("testdata/twitter.json", .{ .read = true });
    defer f.close();

    const Status = struct { id: usize };
    const Twitter = struct { statuses: []const Status };
    const result = try json.parse(Twitter, f.reader(), .{ .allocator = testing.allocator });
    defer json.parseFree(Twitter, result, .{ .allocator = testing.allocator });
    try testing.expectEqual(@as(usize, 1), result.statuses.len);
    try testing.expectEqual(@as(usize, 505874924095815681), result.statuses[0].id);
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

        pub fn on_custom(_: *mem.Allocator, p: anytype) !u7 {
            _ = try p.nextToken(p.token_buf.len);
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
    const options = Options{ .allocator = testing.allocator };
    const result = try json.parseText(T, input, options);
    defer json.parseFree(T, result, options);

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
    const T = struct { b: u8 };
    try testing.expectError(error.UnknownField, json.parseText(T,
        \\{"a":1}
    , .{ .allocator = testing.allocator, .skip_unknown_fields = false }));
}

test "comptime fields" {
    {
        const T = struct { comptime a: u8 = 44 };
        try testing.expectError(error.UnexpectedValue, json.parseText(T,
            \\{"a":1}
        , .{
            .allocator = testing.allocator,
            .skip_unknown_fields = false,
        }));
    }
    {
        const T = struct { comptime b: f32 = 1.1 };
        try testing.expectError(error.UnexpectedValue, json.parseText(T,
            \\{"b": 2.2}
        , .{
            .allocator = testing.allocator,
            .skip_unknown_fields = false,
        }));
    }
    {
        const T = struct {
            comptime a: u8 = 44,
            comptime b: f32 = 1.1,
        };
        _ = try json.parseText(T,
            \\{"a": 44, "b": 1.1}
        , .{
            .allocator = testing.allocator,
            .skip_unknown_fields = false,
        });
        // don't need to verify field values, just make sure the fields are found
    }
}
