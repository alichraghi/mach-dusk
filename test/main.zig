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

// TODO: move this to cli/main.zig
pub fn printErrors(errors: []*dusk.ErrorMsg, source: []const u8, file_path: ?[]const u8) !void {
    var bw = std.io.bufferedWriter(std.io.getStdErr().writer());
    const b = bw.writer();
    const term = std.debug.TTY.Config{ .escape_codes = {} };

    for (errors) |err| {
        defer err.deinit(allocator);

        const loc = err.loc.?;
        const loc_extra = loc.extraInfo(source);

        // 'file:line:column'
        try term.setColor(b, .Bold);
        try b.print("{?s}:{d}:{d} ", .{ file_path, loc_extra.line, loc_extra.col });

        // 'error: '
        try term.setColor(b, .Red);
        try b.writeAll("error: ");

        // error message
        try term.setColor(b, .Reset);
        try term.setColor(b, .Bold);
        try b.writeAll(err.msg);
        try b.writeByte('\n');

        // error code
        try printCode(b, term, source, loc);

        // note
        if (err.note) |note| {
            try term.setColor(b, .Cyan);
            try b.writeAll("note: ");

            // note message
            try term.setColor(b, .Reset);
            try term.setColor(b, .Bold);
            try b.writeAll(note.msg);
            try b.writeByte('\n');

            if (note.loc) |note_loc| {
                // note code
                try printCode(b, term, source, note_loc);
            }
        }

        // clean up and flush
        try term.setColor(b, .Reset);
        try bw.flush();
    }
}

fn printCode(writer: anytype, term: std.debug.TTY.Config, source: []const u8, loc: dusk.Token.Loc) !void {
    const loc_extra = loc.extraInfo(source);
    try term.setColor(writer, .Dim);
    try writer.print("{d} │ ", .{loc_extra.line});
    try term.setColor(writer, .Reset);
    try writer.writeAll(source[loc_extra.line_start..loc.start]);
    try term.setColor(writer, .Green);
    try writer.writeAll(source[loc.start..loc.end]);
    try term.setColor(writer, .Reset);
    try writer.writeAll(source[loc.end..loc_extra.line_end]);
    try writer.writeByte('\n');

    // location pointer
    const line_number_len = (std.math.log10(loc_extra.line) + 1) + 3;
    try writer.writeByteNTimes(
        ' ',
        line_number_len + (loc_extra.col - 1),
    );
    try term.setColor(writer, .Bold);
    try term.setColor(writer, .Green);
    try writer.writeByte('^');
    try writer.writeByteNTimes('~', loc.end - loc.start - 1);
    try writer.writeByte('\n');
}

fn expectTree(source: [:0]const u8) !dusk.Ast {
    var res = try dusk.Ast.parse(allocator, source);
    switch (res) {
        .tree => |tree| {
            if (try tree.resolve(allocator)) |errors| {
                try printErrors(errors, source, null);
                allocator.free(errors);
            }
            return tree;
        },
        .errors => |err_msgs| {
            try printErrors(err_msgs, source, null);
        },
    }

    return error.UnexpectedResult;
}

test "empty" {
    const source = "";
    var tree = try expectTree(source);
    defer tree.deinit(allocator);
}

test "boids" {
    const source = try readTestFile("boids.wgsl");
    defer allocator.free(source);
    var tree = try expectTree(source);
    defer tree.deinit(allocator);
}

test "gkurve" {
    if (true) return error.SkipZigTest;

    const source = try readTestFile("gkurve.wgsl");
    defer allocator.free(source);

    var tree = try expectTree(source);
    defer tree.deinit(allocator);
}

test "variable & expressions" {
    const source = "var expr = 1 + 5 + 2 * 3 > 6 >> 7;";

    var tree = try expectTree(source);
    defer tree.deinit(allocator);

    const root_node = 0;
    try expect(tree.nodeLHS(root_node) + 1 == tree.nodeRHS(root_node));

    const variable = tree.spanToList(root_node)[0];
    const variable_name = tree.tokenLoc(tree.extraData(dusk.Ast.Node.GlobalVarDecl, tree.nodeLHS(variable)).name);
    try expect(std.mem.eql(u8, "expr", variable_name.slice(source)));
    try expect(tree.nodeTag(variable) == .global_variable);
    try expect(tree.tokenTag(tree.nodeToken(variable)) == .k_var);

    const expr = tree.nodeRHS(variable);
    try expect(tree.nodeTag(expr) == .greater);

    const @"1 + 5 + 2 * 3" = tree.nodeLHS(expr);
    try expect(tree.nodeTag(@"1 + 5 + 2 * 3") == .add);

    const @"1 + 5" = tree.nodeLHS(@"1 + 5 + 2 * 3");
    try expect(tree.nodeTag(@"1 + 5") == .add);

    const @"1" = tree.nodeLHS(@"1 + 5");
    try expect(tree.nodeTag(@"1") == .number_literal);

    const @"5" = tree.nodeRHS(@"1 + 5");
    try expect(tree.nodeTag(@"5") == .number_literal);

    const @"2 * 3" = tree.nodeRHS(@"1 + 5 + 2 * 3");
    try expect(tree.nodeTag(@"2 * 3") == .mul);

    const @"6 >> 7" = tree.nodeRHS(expr);
    try expect(tree.nodeTag(@"6 >> 7") == .shift_right);

    const @"6" = tree.nodeLHS(@"6 >> 7");
    try expect(tree.nodeTag(@"6") == .number_literal);

    const @"7" = tree.nodeRHS(@"6 >> 7");
    try expect(tree.nodeTag(@"7") == .number_literal);
}
