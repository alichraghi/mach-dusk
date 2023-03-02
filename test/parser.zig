const std = @import("std");
const dusk = @import("dusk");
const Ast = dusk.Ast;
const expect = std.testing.expect;

const max_file_size = 1024 * 1024;

test "empty" {
    const source = "";
    var ast = try Ast.parse(std.testing.allocator, source);
    defer ast.deinit(std.testing.allocator);
}

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
}

test "gkurve" {
    if (true) return error.SkipZigTest;
    const source = try std.fs.cwd().readFileAllocOptions(
        std.testing.allocator,
        "test/gkurve.wgsl",
        max_file_size,
        null,
        @alignOf(u8),
        0,
    );
    defer std.testing.allocator.free(source);
    var ast = try Ast.parse(std.testing.allocator, source);
    defer ast.deinit(std.testing.allocator);
}

test "variable & expressions" {
    const source = "var expr = 1 + 5 + 2 * 3 > 6 >> 7;";

    var ast = try Ast.parse(std.testing.allocator, source);
    defer ast.deinit(std.testing.allocator);

    const root = ast.nodes.get(0);
    try expect(root.lhs + 1 == root.rhs);

    const @"var expr = 1 + 5 + 2 * 3 > 6 >> 7" = ast.nodes.get(ast.extra_data.items[root.lhs]);
    const expr = ast.tokens.get(ast.extra_data.items[@"var expr = 1 + 5 + 2 * 3 > 6 >> 7".lhs + 1]);
    try expect(std.mem.eql(u8, "expr", expr.loc.asStr(source)));
    try expect(@"var expr = 1 + 5 + 2 * 3 > 6 >> 7".tag == .global_variable);
    try expect(ast.tokens.get(@"var expr = 1 + 5 + 2 * 3 > 6 >> 7".main_token).tag == .keyword_var);

    const @"1 + 5 + 2 * 3 > 6 >> 7" = ast.nodes.get(@"var expr = 1 + 5 + 2 * 3 > 6 >> 7".rhs);
    try expect(@"1 + 5 + 2 * 3 > 6 >> 7".tag == .greater);

    const @"1 + 5 + 2 * 3" = ast.nodes.get(@"1 + 5 + 2 * 3 > 6 >> 7".lhs);
    try expect(@"1 + 5 + 2 * 3".tag == .add);

    const @"1 + 5" = ast.nodes.get(@"1 + 5 + 2 * 3".lhs);
    try expect(@"1 + 5".tag == .add);

    const @"1" = ast.nodes.get(@"1 + 5".lhs);
    try expect(@"1".tag == .number_literal);

    const @"5" = ast.nodes.get(@"1 + 5".rhs);
    try expect(@"5".tag == .number_literal);

    const @"2 * 3" = ast.nodes.get(@"1 + 5 + 2 * 3".rhs);
    try expect(@"2 * 3".tag == .mul);

    const @"6 >> 7" = ast.nodes.get(@"1 + 5 + 2 * 3 > 6 >> 7".rhs);
    try expect(@"6 >> 7".tag == .shift_right);

    const @"6" = ast.nodes.get(@"6 >> 7".lhs);
    try expect(@"6".tag == .number_literal);

    const @"7" = ast.nodes.get(@"6 >> 7".rhs);
    try expect(@"7".tag == .number_literal);
}
