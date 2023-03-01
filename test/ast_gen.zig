const std = @import("std");
const dusk = @import("dusk");
const Ast = dusk.Ast;
const IR = dusk.IR;
const expect = std.testing.expect;

const max_file_size = 1024 * 1024;

test "boids" {
    const source = try std.fs.cwd().readFileAllocOptions(
        std.testing.allocator,
        "test/boids.wgsl",
        max_file_size,
        null,
        @alignOf(u8),
        0,
    );
    defer std.testing.allocator.free(source);
    var ast = try Ast.parse(std.testing.allocator, source);
    defer ast.deinit(std.testing.allocator);

    _ = try IR.generate(ast);
}
