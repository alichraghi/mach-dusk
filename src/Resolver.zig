const std = @import("std");
const Ast = @import("Ast.zig");
const ErrorList = @import("ErrorList.zig");
const assert = std.debug.assert;
const Resolver = @This();

ast: *const Ast,
error_list: ErrorList,

pub fn resolve(allocator: std.mem.Allocator, ast: Ast) !void {
    var resolver = Resolver{
        .ast = &ast,
        .error_list = .{
            .allocator = allocator,
            .source = ast.source,
        },
    };
    defer resolver.error_list.deinit();

    const root_node = resolver.ast.nodes.get(0);

    assert(root_node.tag == .span);

    std.debug.print("\n{d}..{d}\n", .{ root_node.lhs, root_node.rhs });

    for (root_node.lhs..root_node.rhs) |node_index| {
        try resolver.globalDecl(
            resolver.ast.extra_data.items[@intCast(Ast.Node.Index, node_index)],
        );
    }
}

pub fn globalDecl(self: *Resolver, node_index: Ast.Node.Index) !void {
    const node = self.ast.nodes.get(node_index);

    switch (node.tag) {
        .global_variable => std.debug.print("\nVAR\n", .{}),
        else => std.debug.print("{}\n", .{node.tag}),
    }
}
