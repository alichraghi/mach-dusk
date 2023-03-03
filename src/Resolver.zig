const std = @import("std");
const Ast = @import("Ast.zig");
const ErrorList = @import("ErrorList.zig");
const Token = @import("Token.zig");
const assert = std.debug.assert;
const Resolver = @This();

ast: *const Ast,
error_list: ErrorList,

pub fn deinit(self: *Resolver) void {
    self.error_list.deinit();
}

pub fn resolveRoot(self: *Resolver) !void {
    const global_items = self.ast.spanToList(0);

    for (global_items, 0..) |node_i, i| {
        try self.checkRedeclaration(global_items[i + 1 ..], node_i);
        try self.globalDecl(global_items, node_i);
    }

    if (self.error_list.errors.items.len > 0) {
        try self.error_list.flush();
    }
}

pub fn globalDecl(self: *Resolver, parent_scope: []const Ast.Index, node_i: Ast.Index) !void {
    switch (self.ast.nodeTag(node_i)) {
        .global_variable => {}, // TODO
        .struct_decl => try self.structDecl(parent_scope, node_i),
        else => std.debug.print("Global Decl TODO: {}\n", .{self.ast.nodeTag(node_i)}),
    }
}

pub fn structDecl(self: *Resolver, parent_scope: []const Ast.Index, node: Ast.Index) !void {
    const member_list = self.ast.spanToList(self.ast.nodeLHS(node));
    for (member_list, 0..) |member_node, i| {
        try self.checkRedeclaration(member_list[i + 1 ..], member_node);

        const member_loc = self.ast.tokenLoc(self.ast.nodeToken(member_node));
        const member_name = member_loc.slice(self.ast.source);
        const member_type_node = self.ast.nodeRHS(member_node);

        switch (self.ast.nodeTag(member_type_node)) {
            .scalar_type,
            .vector_type,
            .matrix_type,
            .atomic_type,
            => {},
            .array_type => if (self.ast.nodeRHS(member_type_node) == Ast.null_index and i != member_list.len - 1) {
                try self.error_list.add(
                    member_loc,
                    "struct member with runtime-sized array type, must be the last member of the structure",
                    .{},
                    null,
                    null,
                );
            },
            .user_type => {
                _ = self.findDeclNode(parent_scope, member_name) orelse {
                    try self.error_list.add(
                        member_loc,
                        "use of undeclared identifier '{s}'",
                        .{member_name},
                        null,
                        null,
                    );
                    continue;
                };
            },
            else => {
                try self.error_list.add(
                    member_loc,
                    "invalid struct member type '{s}'",
                    .{member_name},
                    null,
                    null,
                );
            },
        }
    }
}

pub fn findDeclNode(self: *Resolver, scope_items: []const Ast.Index, name: []const u8) ?Ast.Index {
    for (scope_items) |node| {
        const node_token = self.declNameToken(node) orelse continue;
        if (std.mem.eql(u8, name, self.ast.tokenLoc(node_token).slice(self.ast.source))) {
            return node;
        }
    }
    return null;
}

pub fn checkRedeclaration(self: *Resolver, scope_items: []const Ast.Index, decl_node: Ast.Index) !void {
    const decl_token_loc = self.ast.tokenLoc(self.declNameToken(decl_node).?);
    const decl_name = decl_token_loc.slice(self.ast.source);
    for (scope_items) |redecl_node| {
        assert(decl_node != redecl_node);
        const redecl_token_loc = self.ast.tokenLoc(self.declNameToken(redecl_node).?);
        const redecl_name = redecl_token_loc.slice(self.ast.source);
        if (std.mem.eql(u8, decl_name, redecl_name)) {
            try self.error_list.add(
                redecl_token_loc,
                "redeclaration of '{s}'",
                .{decl_name},
                null,
                null,
            );
        }
    }
}

pub fn declNameToken(self: *Resolver, node: Ast.Index) ?Ast.Index {
    return switch (self.ast.nodeTag(node)) {
        .global_variable => self.ast.extraData(Ast.Node.GlobalVarDecl, self.ast.nodeLHS(node)).name,
        .struct_decl,
        .fn_decl,
        .global_constant,
        .override,
        .type_alias,
        => self.ast.nodeToken(node) + 1,
        .struct_member => self.ast.nodeToken(node),
        else => null,
    };
}
