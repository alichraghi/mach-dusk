const std = @import("std");
const dusk = @import("dusk");
const expect = std.testing.expect;
const allocator = std.testing.allocator;

fn readTestFile(comptime path: []const u8) ![:0]const u8 {
    const max_file_size = 1024 * 1024;
    return std.fs.cwd().readFileAllocOptions(
        allocator,
        "test/" ++ path,
        max_file_size,
        null,
        @alignOf(u8),
        0,
    );
}

test "empty" {
    const source = "";

    var ast = try dusk.Ast.parse(allocator, source);
    defer ast.deinit(allocator);

    try ast.resolve(allocator);
}

test "boids" {
    const source = try readTestFile("boids.wgsl");
    defer allocator.free(source);

    var ast = try dusk.Ast.parse(allocator, source);
    defer ast.deinit(allocator);

    try ast.resolve(allocator);
}

test "gkurve" {
    if (true) return error.SkipZigTest;

    const source = try readTestFile("gkurve.wgsl");
    defer allocator.free(source);

    var ast = try dusk.Ast.parse(allocator, source);
    defer ast.deinit(allocator);

    try ast.resolve(allocator);
}

test "variable & expressions" {
    const source = "var expr = 1 + 5 + 2 * 3 > 6 >> 7;";

    var ast = try dusk.Ast.parse(allocator, source);
    defer ast.deinit(allocator);

    const root_node = 0;
    try expect(ast.nodeLHS(root_node) + 1 == ast.nodeRHS(root_node));

    const variable = ast.spanToList(root_node)[0];
    const variable_name = ast.tokenLoc(ast.extraData(dusk.Ast.Node.GlobalVarDecl, ast.nodeLHS(variable)).name);
    try expect(std.mem.eql(u8, "expr", variable_name.slice(source)));
    try expect(ast.nodeTag(variable) == .global_variable);
    try expect(ast.tokenTag(ast.nodeToken(variable)) == .k_var);

    const expr = ast.nodeRHS(variable);
    try expect(ast.nodeTag(expr) == .greater);

    const @"1 + 5 + 2 * 3" = ast.nodeLHS(expr);
    try expect(ast.nodeTag(@"1 + 5 + 2 * 3") == .add);

    const @"1 + 5" = ast.nodeLHS(@"1 + 5 + 2 * 3");
    try expect(ast.nodeTag(@"1 + 5") == .add);

    const @"1" = ast.nodeLHS(@"1 + 5");
    try expect(ast.nodeTag(@"1") == .number_literal);

    const @"5" = ast.nodeRHS(@"1 + 5");
    try expect(ast.nodeTag(@"5") == .number_literal);

    const @"2 * 3" = ast.nodeRHS(@"1 + 5 + 2 * 3");
    try expect(ast.nodeTag(@"2 * 3") == .mul);

    const @"6 >> 7" = ast.nodeRHS(expr);
    try expect(ast.nodeTag(@"6 >> 7") == .shift_right);

    const @"6" = ast.nodeLHS(@"6 >> 7");
    try expect(ast.nodeTag(@"6") == .number_literal);

    const @"7" = ast.nodeRHS(@"6 >> 7");
    try expect(ast.nodeTag(@"7") == .number_literal);
}
