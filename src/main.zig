const std = @import("std");
const mem = std.mem;
const json = @import("simdjson.zig");

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

    var res = try json.parse([]const struct { a: u8 }, f.reader(), .{
        .allocator = allocator,
        .skip_unknown_fields = true,
    });
    _ = res;
    // try testing.expectEqual(@as(usize, 100), res.statuses.len);
    // std.debug.print("res.statuses.len {} last status {}\n", .{ res.statuses.len, res.statuses[res.statuses.len - 1] });
    // allocator.free(res.statuses);
    // try testing.expectEqual(@as(usize, 505874924095815681), res.statuses[0].id);
}
