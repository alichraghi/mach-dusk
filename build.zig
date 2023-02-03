const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const mode = b.standardReleaseOptions();
    const target = b.standardTargetOptions(.{});
    const main_tests = b.addTestExe("test", "src/main.zig");
    main_tests.setTarget(target);
    main_tests.setBuildMode(mode);
    main_tests.install();
    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.run().step);
}
