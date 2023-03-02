const std = @import("std");
const Ast = @import("Ast.zig");
const ErrorList = @import("ErrorList.zig");
const Token = @import("Token.zig");
const assert = std.debug.assert;
const Resolver = @This();

arena: std.mem.Allocator,
ast: *const Ast,
error_list: ErrorList,

pub fn resolve(allocator: std.mem.Allocator, ast: Ast) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    var resolver = Resolver{
        .arena = arena.allocator(),
        .ast = &ast,
        .error_list = .{
            .allocator = allocator,
            .source = ast.source,
        },
    };
    defer {
        arena.deinit();
        resolver.error_list.deinit();
    }

    const global_items = resolver.spanToList(0);

    for (global_items, 0..) |node_i, i| {
        try resolver.checkRedeclaration(global_items[i + 1 ..], node_i);
        try resolver.globalDecl(global_items, node_i);
    }

    if (resolver.error_list.errors.items.len > 0) {
        try resolver.error_list.flush();
    }
}

pub fn globalDecl(self: *Resolver, parent_scope: []const Ast.Node.Index, node_i: Ast.Node.Index) !void {
    const node = self.ast.nodes.get(node_i);

    switch (node.tag) {
        .global_variable => {}, // TODO
        .struct_decl => try self.structDecl(parent_scope, node_i),
        else => std.debug.print("Global Decl TODO: {}\n", .{node.tag}),
    }
}

pub fn structDecl(self: *Resolver, parent_scope: []const Ast.Node.Index, node_i: Ast.Node.Index) !void {
    const node = self.ast.nodes.get(node_i);

    const member_list = self.spanToList(node.lhs);
    for (member_list, 0..) |member_i, i| {
        try self.checkRedeclaration(member_list[i + 1 ..], member_i);
        const member = self.ast.nodes.get(member_i);
        const member_type = self.ast.nodes.get(member.rhs);
        const member_token = self.ast.tokens.get(member_type.main_token);
        const member_name = member_token.loc.asStr(self.ast.source);
        switch (member_type.tag) {
            .scalar_type,
            .vector_type,
            .matrix_type,
            .atomic_type,
            => {},
            .array_type => if (member_type.rhs == Ast.null_index and i != member_list.len - 1) {
                try self.error_list.add(
                    member_token.loc,
                    "runtime-sized array type, must be the last member of the structure",
                    .{},
                    &.{},
                );
            },
            .user_type => {
                _ = self.findDeclNode(parent_scope, member_name) orelse {
                    try self.error_list.add(
                        member_token.loc,
                        "use of undeclared identifier '{s}'",
                        .{member_name},
                        &.{},
                    );
                    continue;
                };
            },
            else => {
                try self.error_list.add(
                    member_token.loc,
                    "invalid struct member type '{s}'",
                    .{member_name},
                    &.{},
                );
            },
        }
    }
}

pub fn findDeclNode(self: *Resolver, scope_items: []const Ast.Node.Index, name: []const u8) ?Ast.Node.Index {
    for (scope_items) |node| {
        const node_token = self.declNameToken(node) orelse continue;
        if (std.mem.eql(u8, name, node_token.loc.asStr(self.ast.source))) {
            return node;
        }
    }
    return null;
}

pub fn checkRedeclaration(self: *Resolver, scope_items: []const Ast.Node.Index, decl_node_i: Ast.Node.Index) !void {
    const decl_token = self.declNameToken(decl_node_i).?;
    const decl_name = decl_token.loc.asStr(self.ast.source);
    for (scope_items) |redecl_node_i| {
        const redecl_token = self.declNameToken(redecl_node_i).?;
        const redecl_name = redecl_token.loc.asStr(self.ast.source);
        if (decl_node_i != redecl_node_i and std.mem.eql(u8, decl_name, redecl_name)) {
            try self.error_list.add(
                redecl_token.loc,
                "redeclaration of '{s}'",
                .{decl_name},
                &.{
                    try std.fmt.allocPrint(self.arena, "first declared here", .{}),
                }, // TODO
            );
        }
    }
}

pub fn declNameToken(self: *Resolver, node_i: Ast.Node.Index) ?Token {
    const node = self.ast.nodes.get(node_i);
    return switch (node.tag) {
        .global_variable => self.ast.tokens.get(self.extraData(node.lhs, Ast.Node.GlobalVarDecl).name),
        .struct_decl,
        .fn_decl,
        .global_constant,
        .global_override,
        .type_alias,
        => self.ast.tokens.get(node.main_token + 1),
        .struct_member => self.ast.tokens.get(node.main_token),
        else => null,
    };
}

pub fn spanToList(self: *Resolver, span: Ast.Node.Index) []const Ast.Node.Index {
    const node = self.ast.nodes.get(span);
    assert(node.tag == .span);
    return self.ast.extra_data.items[node.lhs..node.rhs];
}

pub fn extraData(self: *Resolver, index: Ast.Node.Index, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        comptime assert(field.type == Ast.Node.Index);
        @field(result, field.name) = self.ast.extra_data.items[index + i];
    }
    return result;
}
