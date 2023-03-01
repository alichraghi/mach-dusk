const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const optimize = b.standardOptimizeOption(.{});
    const main_tests = b.addTest(.{
        .kind = .test_exe,
        .root_source_file = .{ .path = "test/main.zig" },
        .optimize = optimize,
    });
    main_tests.addModule("dusk", module(b));
    main_tests.linkLibC();
    main_tests.install();
    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.run().step);
}

pub fn module(b: *std.build.Builder) *std.build.Module {
    return b.createModule(.{ .source_file = .{ .path = "src/main.zig" } });
}
