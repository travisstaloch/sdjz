const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    // const lib = b.addStaticLibrary("simdjson-zig", "src/main.zig");
    // lib.setBuildMode(mode);
    // lib.install();

    var main_tests = b.addTest("src/tests.zig");
    main_tests.setBuildMode(mode);
    main_tests.linkLibC();

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);

    const exe = b.addExecutable("simdjson-zig", "src/main.zig");
    exe.setBuildMode(mode);
    exe.install();
    exe.linkLibC();
}
