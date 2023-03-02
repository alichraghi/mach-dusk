//! Based on <SPEC_COMMIT>
const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");
const ErrorList = @import("ErrorList.zig");
const comptimePrint = std.fmt.comptimePrint;
const fieldNames = std.meta.fieldNames;
const Parser = @This();

allocator: std.mem.Allocator,
ast: Ast,
tok_i: Ast.Index = 0,
error_list: ErrorList,
scratch: std.ArrayListUnmanaged(Ast.Index) = .{},

pub fn expectGlobalDeclRecoverable(p: *Parser) !?Ast.Index {
    return p.expectGlobalDecl() catch |err| switch (err) {
        error.Parsing => {
            p.findNextGlobal();
            return null;
        },
        else => return err,
    };
}

pub fn expectGlobalDecl(p: *Parser) !Ast.Index {
    while (p.eatToken(.semicolon)) |_| {}

    const attrs = try p.attributeList();

    if (try p.structDecl() orelse
        try p.functionDecl(attrs)) |node|
    {
        return node;
    }

    // decl SEMICOLON
    if (try p.globalConstDecl() orelse
        try p.typeAliasDecl() orelse
        try p.constAssert() orelse
        try p.globalVarDecl(attrs) orelse
        try p.globalOverrideDecl(attrs)) |node|
    {
        _ = try p.expectToken(.semicolon);
        return node;
    }

    try p.error_list.add(
        p.peekToken(0).loc,
        "expected global declaration, found '{s}'",
        .{p.peekToken(0).tag.symbol()},
        &.{},
    );
    return error.Parsing;
}

pub fn attributeList(p: *Parser) !?Ast.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const attr = try p.attribute() orelse break;
        try p.scratch.append(p.allocator, attr);
    }
    const list = p.scratch.items[scratch_top..];
    if (list.len == 0) return null;
    return try p.listToSpan(list);
}

pub fn attribute(p: *Parser) !?Ast.Index {
    const attr_token = p.eatToken(.attr) orelse return null;
    const ident_tok = try p.expectToken(.ident);
    const str = p.ast.getToken(ident_tok).loc.slice(p.ast.source);
    const tag = std.meta.stringToEnum(Ast.Attribute, str) orelse {
        try p.error_list.add(
            p.ast.getToken(ident_tok).loc,
            "unknown attribute '{s}'",
            .{p.ast.getToken(ident_tok).loc.slice(p.ast.source)},
            comptime &.{comptimePrint("valid options are [{s}]", .{fieldNames(Ast.Attribute)})},
        );
        return error.Parsing;
    };
    var node = Ast.Node{
        .tag = undefined,
        .main_token = attr_token,
    };
    switch (tag) {
        .invariant,
        .@"const",
        .vertex,
        .fragment,
        .compute,
        => node.tag = .attr,
        .@"align",
        .binding,
        .group,
        .id,
        .location,
        .size,
        .builtin,
        => {
            _ = try p.expectToken(.paren_left);
            if (tag == .builtin) {
                node.tag = .attr_builtin;
                node.lhs = try p.expectBuiltinValue();
            } else {
                node.tag = .attr_one_arg;
                node.lhs = try p.expression() orelse {
                    try p.error_list.add(
                        p.peekToken(0).loc,
                        "expected expression, but found '{s}'",
                        .{p.peekToken(0).tag.symbol()},
                        &.{},
                    );
                    return error.Parsing;
                };
            }
            _ = p.eatToken(.comma);
            _ = try p.expectToken(.paren_right);
        },
        .workgroup_size => {
            _ = try p.expectToken(.paren_left);

            node.tag = .attr_workgroup_size;
            var workgroup_size = Ast.Node.WorkgroupSize{
                .x = try p.expression() orelse {
                    try p.error_list.add(p.peekToken(0).loc, "expected workgroup_size x parameter", .{}, &.{});
                    return error.Parsing;
                },
            };

            if (p.eatToken(.comma) != null and p.peekToken(0).tag != .paren_right) {
                workgroup_size.y = try p.expression() orelse {
                    try p.error_list.add(p.peekToken(0).loc, "expected workgroup_size y parameter", .{}, &.{});
                    return error.Parsing;
                };

                if (p.eatToken(.comma) != null and p.peekToken(0).tag != .paren_right) {
                    workgroup_size.z = try p.expression() orelse {
                        try p.error_list.add(p.peekToken(0).loc, "expected workgroup_size z parameter", .{}, &.{});
                        return error.Parsing;
                    };

                    _ = p.eatToken(.comma);
                }
            }

            node.lhs = try p.addExtra(workgroup_size);
            _ = try p.expectToken(.paren_right);
        },
        .interpolate => {
            _ = try p.expectToken(.paren_left);

            node.tag = .attr_interpolate;
            node.lhs = try p.expectInterpolationType();

            if (p.eatToken(.comma) != null and p.peekToken(0).tag != .paren_right) {
                node.rhs = try p.expectInterpolationSample();
                _ = p.eatToken(.comma);
                _ = try p.expectToken(.paren_right);
            }

            _ = try p.expectToken(.paren_right);
        },
    }

    return try p.addNode(node);
}

pub fn expectBuiltinValue(p: *Parser) !Ast.Index {
    const token = p.advanceToken();
    if (p.ast.getToken(token).tag == .ident) {
        const str = p.ast.getToken(token).loc.slice(p.ast.source);
        if (std.meta.stringToEnum(Ast.BuiltinValue, str)) |_| return token;
    }

    try p.error_list.add(
        p.ast.getToken(token).loc,
        "unknown builtin value name '{s}'",
        .{p.ast.getToken(token).loc.slice(p.ast.source)},
        comptime &.{comptimePrint("valid options are [{s}]", .{fieldNames(Ast.BuiltinValue)})},
    );
    return error.Parsing;
}

pub fn expectInterpolationType(p: *Parser) !Ast.Index {
    const token = p.advanceToken();
    if (p.ast.getToken(token).tag == .ident) {
        const str = p.ast.getToken(token).loc.slice(p.ast.source);
        if (std.meta.stringToEnum(Ast.InterpolationType, str)) |_| return token;
    }

    try p.error_list.add(
        p.ast.getToken(token).loc,
        "unknown interpolation type name '{s}'",
        .{p.ast.getToken(token).loc.slice(p.ast.source)},
        comptime &.{comptimePrint("valid options are [{s}]", .{fieldNames(Ast.InterpolationType)})},
    );
    return error.Parsing;
}

pub fn expectInterpolationSample(p: *Parser) !Ast.Index {
    const token = p.advanceToken();
    if (p.ast.getToken(token).tag == .ident) {
        const str = p.ast.getToken(token).loc.slice(p.ast.source);
        if (std.meta.stringToEnum(Ast.InterpolationSample, str)) |_| return token;
    }

    try p.error_list.add(
        p.ast.getToken(token).loc,
        "unknown interpolation sample name '{s}'",
        .{p.ast.getToken(token).loc.slice(p.ast.source)},
        comptime &.{comptimePrint("valid options are [{s}]", .{fieldNames(Ast.InterpolationSample)})},
    );
    return error.Parsing;
}

pub fn globalVarDecl(p: *Parser, attrs: ?Ast.Index) !?Ast.Index {
    const var_token = p.eatToken(.k_var) orelse return null;

    // qualifier
    var addr_space = Ast.null_index;
    var access_mode = Ast.null_index;
    if (p.eatToken(.less_than)) |_| {
        addr_space = try p.expectAddressSpace();
        access_mode = if (p.eatToken(.comma)) |_|
            try p.expectAccessMode()
        else
            Ast.null_index;
        _ = try p.expectToken(.greater_than);
    }

    // name, type
    const name_token = try p.expectToken(.ident);
    var var_type = Ast.null_index;
    if (p.eatToken(.colon)) |_| {
        var_type = try p.expectTypeSpecifier();
    }

    var initializer = Ast.null_index;
    if (p.eatToken(.equal)) |_| {
        initializer = try p.expression() orelse {
            try p.error_list.add(
                p.peekToken(0).loc,
                "expected initializer expression, found '{s}'",
                .{p.peekToken(0).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
    }

    const extra = try p.addExtra(Ast.Node.GlobalVarDecl{
        .attrs = attrs orelse Ast.null_index,
        .name = name_token,
        .addr_space = addr_space,
        .access_mode = access_mode,
        .type = var_type,
    });
    return try p.addNode(.{
        .tag = .global_variable,
        .main_token = var_token,
        .lhs = extra,
        .rhs = initializer,
    });
}

pub fn globalConstDecl(p: *Parser) !?Ast.Index {
    const const_token = p.eatToken(.k_const) orelse return null;

    _ = try p.expectToken(.ident);
    var const_type = Ast.null_index;
    if (p.eatToken(.colon)) |_| {
        const_type = try p.expectTypeSpecifier();
    }

    _ = try p.expectToken(.equal);
    const initializer = try p.expression() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "expected initializer expression, found '{s}'",
            .{p.peekToken(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };

    return try p.addNode(.{
        .tag = .global_constant,
        .main_token = const_token,
        .lhs = const_type,
        .rhs = initializer,
    });
}

pub fn globalOverrideDecl(p: *Parser, attrs: ?Ast.Index) !?Ast.Index {
    const override_token = p.eatToken(.k_override) orelse return null;

    // name, type
    _ = try p.expectToken(.ident);
    var override_type = Ast.null_index;
    if (p.eatToken(.colon)) |_| {
        override_type = try p.expectTypeSpecifier();
    }

    var initializer = Ast.null_index;
    if (p.eatToken(.equal)) |_| {
        initializer = try p.expression() orelse {
            try p.error_list.add(
                p.peekToken(0).loc,
                "expected initializer expression, found '{s}'",
                .{p.peekToken(0).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
    }

    const extra = try p.addExtra(Ast.Node.OverrideDecl{
        .attrs = attrs orelse Ast.null_index,
        .type = override_type,
    });
    return try p.addNode(.{
        .tag = .override,
        .main_token = override_token,
        .lhs = extra,
        .rhs = initializer,
    });
}

pub fn typeAliasDecl(p: *Parser) !?Ast.Index {
    const type_token = p.eatToken(.k_type) orelse return null;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.equal);
    const value = try p.expectTypeSpecifier();
    return try p.addNode(.{
        .tag = .type_alias,
        .main_token = type_token,
        .lhs = value,
    });
}

pub fn structDecl(p: *Parser) !?Ast.Index {
    const main_token = p.eatToken(.k_struct) orelse return null;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.brace_left);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const attrs = try p.attributeList();
        const member = try p.structMember(attrs) orelse {
            if (attrs != null) {
                try p.error_list.add(
                    p.peekToken(0).loc,
                    "expected struct member, found '{s}'",
                    .{p.peekToken(0).tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            }
            break;
        };
        try p.scratch.append(p.allocator, member);
        _ = p.eatToken(.comma);
    }

    _ = try p.expectToken(.brace_right);

    const list = p.scratch.items[scratch_top..];
    const members = try p.listToSpan(list);

    return try p.addNode(.{
        .tag = .struct_decl,
        .main_token = main_token,
        .lhs = members,
    });
}

pub fn structMember(p: *Parser, attrs: ?Ast.Index) !?Ast.Index {
    const name_token = p.eatToken(.ident) orelse return null;
    _ = try p.expectToken(.colon);
    const member_type = try p.expectTypeSpecifier();
    return try p.addNode(.{
        .tag = .struct_member,
        .main_token = name_token,
        .lhs = attrs orelse Ast.null_index,
        .rhs = member_type,
    });
}

pub fn constAssert(p: *Parser) !?Ast.Index {
    const main_token = p.eatToken(.k_const_assert) orelse return null;
    const expr = try p.expression() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "expected expression, found '{s}'",
            .{p.peekToken(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
    return try p.addNode(.{
        .tag = .const_assert,
        .main_token = main_token,
        .lhs = expr,
    });
}

pub fn functionDecl(p: *Parser, attrs: ?Ast.Index) !?Ast.Index {
    const fn_token = p.eatToken(.k_fn) orelse return null;
    _ = try p.expectToken(.ident);

    _ = try p.expectToken(.paren_left);
    const params = try p.expectParameterList();
    _ = try p.expectToken(.paren_right);

    var result_attrs = Ast.null_index;
    var result_type = Ast.null_index;
    if (p.eatToken(.arrow)) |_| {
        result_attrs = try p.attributeList() orelse Ast.null_index;
        result_type = try p.expectTypeSpecifier();
    }

    const body = try p.block() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "expected function body, found '{s}'",
            .{p.peekToken(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };

    const fn_proto = try p.addExtra(Ast.Node.FnProto{
        .attrs = attrs orelse Ast.null_index,
        .params = params,
        .result_attrs = result_attrs,
        .result_type = result_type,
    });
    return try p.addNode(.{
        .tag = .fn_decl,
        .main_token = fn_token,
        .lhs = fn_proto,
        .rhs = body,
    });
}

pub fn expectParameterList(p: *Parser) !Ast.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const attrs = try p.attributeList();
        const param = try p.parameter(attrs) orelse {
            if (attrs != null) {
                try p.error_list.add(
                    p.peekToken(0).loc,
                    "expected function parameter, found '{s}'",
                    .{p.peekToken(0).tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            }
            break;
        };
        try p.scratch.append(p.allocator, param);
        if (p.eatToken(.comma) == null) break;
    }
    const list = p.scratch.items[scratch_top..];
    return try p.listToSpan(list);
}

pub fn parameter(p: *Parser, attrs: ?Ast.Index) !?Ast.Index {
    const main_token = p.eatToken(.ident) orelse return null;
    _ = try p.expectToken(.colon);
    const param_type = try p.expectTypeSpecifier();
    return try p.addNode(.{
        .tag = .fn_param,
        .main_token = main_token,
        .lhs = attrs orelse Ast.null_index,
        .rhs = param_type,
    });
}

pub fn statementRecoverable(p: *Parser) !?Ast.Index {
    while (true) {
        return p.statement() catch |err| switch (err) {
            error.Parsing => {
                p.findNextStmt();
                switch (p.peekToken(0).tag) {
                    .brace_right => return null,
                    .eof => return error.Parsing,
                    else => continue,
                }
            },
            else => return err,
        };
    }
}

/// for simplicity and better error messages,
/// we are putting all statements here
pub fn statement(p: *Parser) !?Ast.Index {
    while (p.eatToken(.semicolon)) |_| {}

    if (try p.breakStatement() orelse
        try p.breakIfStatement() orelse
        try p.callExpr() orelse
        try p.constAssert() orelse
        try p.continueStatement() orelse
        try p.discardStatement() orelse
        try p.returnStatement() orelse
        try p.varStatement() orelse
        try p.varUpdateStatement()) |node|
    {
        _ = try p.expectToken(.semicolon);
        return node;
    }

    if (try p.block() orelse
        try p.continuingStatement() orelse
        try p.forStatement() orelse
        try p.ifStatement() orelse
        try p.loopStatement() orelse
        try p.switchStatement() orelse
        try p.whileStatement()) |node|
    {
        return node;
    }

    return null;
}

pub fn expectBlock(p: *Parser) error{ OutOfMemory, Parsing }!Ast.Index {
    return try p.block() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "expected block statement, found '{s}'",
            .{p.peekToken(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
}

pub fn block(p: *Parser) error{ OutOfMemory, Parsing }!?Ast.Index {
    _ = p.eatToken(.brace_left) orelse return null;

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    var failed = false;
    while (true) {
        const stmt = try p.statementRecoverable() orelse {
            if (p.peekToken(0).tag == .brace_right) break;
            failed = true;
            try p.error_list.add(
                p.peekToken(0).loc,
                "expected statement, found '{s}'",
                .{p.peekToken(0).tag.symbol()},
                &.{},
            );
            p.findNextStmt();
            continue;
        };
        try p.scratch.append(p.allocator, stmt);
    }
    _ = try p.expectToken(.brace_right);
    if (failed) return error.Parsing;

    const list = p.scratch.items[scratch_top..];
    return try p.listToSpan(list);
}

pub fn breakStatement(p: *Parser) !?Ast.Index {
    const main_token = p.eatToken(.k_break) orelse return null;
    return try p.addNode(.{ .tag = .@"break", .main_token = main_token });
}

pub fn breakIfStatement(p: *Parser) !?Ast.Index {
    if (p.peekToken(0).tag == .k_break and
        p.peekToken(1).tag == .k_if)
    {
        const main_token = p.advanceToken();
        _ = p.advanceToken();
        const cond = try p.expression() orelse {
            try p.error_list.add(
                p.peekToken(0).loc,
                "expected condition expression, found '{s}'",
                .{p.peekToken(0).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
        return try p.addNode(.{
            .tag = .break_if,
            .main_token = main_token,
            .lhs = cond,
        });
    }
    return null;
}

pub fn continueStatement(p: *Parser) !?Ast.Index {
    const main_token = p.eatToken(.k_continue) orelse return null;
    return try p.addNode(.{ .tag = .@"continue", .main_token = main_token });
}

pub fn continuingStatement(p: *Parser) !?Ast.Index {
    const main_token = p.eatToken(.k_continuing) orelse return null;
    const body = try p.expectBlock();
    return try p.addNode(.{
        .tag = .continuing,
        .main_token = main_token,
        .lhs = body,
    });
}

pub fn discardStatement(p: *Parser) !?Ast.Index {
    const main_token = p.eatToken(.k_discard) orelse return null;
    return try p.addNode(.{ .tag = .discard, .main_token = main_token });
}

pub fn forStatement(p: *Parser) !?Ast.Index {
    const main_token = p.eatToken(.k_for) orelse return null;
    _ = try p.expectToken(.paren_left);

    // for init
    const init = try p.callExpr() orelse
        try p.varStatement() orelse
        try p.varUpdateStatement() orelse
        Ast.null_index;
    _ = try p.expectToken(.semicolon);

    const cond = try p.expression() orelse Ast.null_index;
    _ = try p.expectToken(.semicolon);

    // for update
    const update = try p.callExpr() orelse
        try p.varUpdateStatement() orelse
        Ast.null_index;

    _ = try p.expectToken(.paren_right);
    const body = try p.expectBlock();

    const extra = try p.addExtra(Ast.Node.ForHeader{
        .init = init,
        .cond = cond,
        .update = update,
    });
    return try p.addNode(.{
        .tag = .@"for",
        .main_token = main_token,
        .lhs = extra,
        .rhs = body,
    });
}

pub fn ifStatement(p: *Parser) !?Ast.Index {
    const main_token = p.eatToken(.k_if) orelse return null;

    const cond = try p.expression() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "expected condition expression, found '{s}'",
            .{p.peekToken(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
    const body = try p.block() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "expected if body block, found '{s}'",
            .{p.peekToken(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };

    if (p.eatToken(.k_else)) |_| {
        const extra = try p.addExtra(Ast.Node.IfStatement{
            .cond = cond,
            .body = body,
        });

        if (p.peekToken(0).tag == .k_if) {
            const else_if = try p.ifStatement() orelse unreachable;
            return try p.addNode(.{
                .tag = .if_else_if,
                .main_token = main_token,
                .lhs = extra,
                .rhs = else_if,
            });
        }

        const else_body = try p.block() orelse {
            try p.error_list.add(
                p.peekToken(0).loc,
                "expected else body block, found '{s}'",
                .{p.peekToken(0).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };

        return try p.addNode(.{
            .tag = .if_else,
            .main_token = main_token,
            .lhs = extra,
            .rhs = else_body,
        });
    }

    return try p.addNode(.{
        .tag = .@"if",
        .main_token = main_token,
        .lhs = cond,
        .rhs = body,
    });
}

pub fn loopStatement(p: *Parser) !?Ast.Index {
    const main_token = p.eatToken(.k_loop) orelse return null;
    const body = try p.expectBlock();
    return try p.addNode(.{
        .tag = .loop,
        .main_token = main_token,
        .lhs = body,
    });
}

pub fn returnStatement(p: *Parser) !?Ast.Index {
    const main_token = p.eatToken(.k_return) orelse return null;
    const expr = try p.expression() orelse Ast.null_index;
    return try p.addNode(.{
        .tag = .@"return",
        .main_token = main_token,
        .lhs = expr,
    });
}

pub fn switchStatement(p: *Parser) !?Ast.Index {
    const main_token = p.eatToken(.k_switch) orelse return null;

    const expr = try p.expression() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "expected condition expression, found '{s}'",
            .{p.peekToken(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };

    _ = try p.expectToken(.brace_left);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (p.eatToken(.k_default)) |default_token| {
            _ = p.eatToken(.colon);
            const default_body = try p.expectBlock();
            try p.scratch.append(p.allocator, try p.addNode(.{
                .tag = .switch_default,
                .main_token = default_token,
                .lhs = default_body,
            }));
        } else if (p.eatToken(.k_case)) |case_token| {
            const cases_scratch_top = p.scratch.items.len;

            var has_default = false;
            while (true) {
                const case_expr = try p.expression() orelse {
                    if (p.eatToken(.k_default)) |_| continue;
                    break;
                };
                _ = p.eatToken(.comma);
                try p.scratch.append(p.allocator, case_expr);
            }
            const case_expr_list = p.scratch.items[cases_scratch_top..];

            _ = p.eatToken(.colon);
            const default_body = try p.expectBlock();

            try p.scratch.append(p.allocator, try p.addNode(.{
                .tag = if (has_default) .switch_case_default else .switch_case,
                .main_token = case_token,
                .lhs = try p.listToSpan(case_expr_list),
                .rhs = default_body,
            }));
            p.scratch.shrinkRetainingCapacity(cases_scratch_top);
        } else {
            break;
        }
    }

    _ = try p.expectToken(.brace_right);

    const case_list = p.scratch.items[scratch_top..];
    return try p.addNode(.{
        .tag = .@"switch",
        .main_token = main_token,
        .lhs = expr,
        .rhs = try p.listToSpan(case_list),
    });
}

pub fn varStatement(p: *Parser) !?Ast.Index {
    if (p.eatToken(.k_var)) |var_token| {
        var addr_space = Ast.null_index;
        var access_mode = Ast.null_index;
        if (p.eatToken(.less_than)) |_| {
            addr_space = try p.expectAddressSpace();
            access_mode = if (p.eatToken(.comma)) |_|
                try p.expectAccessMode()
            else
                Ast.null_index;
            _ = try p.expectToken(.greater_than);
        }

        const name_token = try p.expectToken(.ident);
        var var_type = Ast.null_index;
        if (p.eatToken(.colon)) |_| {
            var_type = try p.expectTypeSpecifier();
        }

        var initializer = Ast.null_index;
        if (p.eatToken(.equal)) |_| {
            initializer = try p.expression() orelse {
                try p.error_list.add(
                    p.peekToken(0).loc,
                    "expected initializer expression, found '{s}'",
                    .{p.peekToken(0).tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            };
        }

        const extra = try p.addExtra(Ast.Node.VarDecl{
            .name = name_token,
            .addr_space = addr_space,
            .access_mode = access_mode,
            .type = var_type,
        });
        return try p.addNode(.{
            .tag = .var_decl,
            .main_token = var_token,
            .lhs = extra,
            .rhs = initializer,
        });
    }

    const const_let_token = p.eatToken(.k_const) orelse p.eatToken(.k_let) orelse return null;
    _ = try p.expectToken(.ident);
    var const_let_type = Ast.null_index;
    if (p.eatToken(.colon)) |_| {
        const_let_type = try p.expectTypeSpecifier();
    }

    _ = try p.expectToken(.equal);
    const initializer = try p.expression() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "expected initializer expression, found '{s}'",
            .{p.peekToken(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };

    return try p.addNode(.{
        .tag = if (p.ast.getToken(const_let_token).tag == .k_const)
            .const_decl
        else
            .let_decl,
        .main_token = const_let_token,
        .lhs = const_let_type,
        .rhs = initializer,
    });
}

pub fn varUpdateStatement(p: *Parser) !?Ast.Index {
    if (p.eatToken(.underscore)) |_| {
        const equal_token = try p.expectToken(.equal);
        const expr = try p.expression() orelse {
            try p.error_list.add(
                p.peekToken(0).loc,
                "expected expression, found '{s}'",
                .{p.peekToken(0).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
        return try p.addNode(.{
            .tag = .phony_assign,
            .main_token = equal_token,
            .lhs = expr,
        });
    } else if (try p.lhsExpression()) |lhs| {
        const op_token = p.advanceToken();
        switch (p.ast.getToken(op_token).tag) {
            .plus_plus, .minus_minus => {
                return try p.addNode(.{
                    .tag = .increase_decrement,
                    .main_token = op_token,
                    .lhs = lhs,
                });
            },
            .equal,
            .plus_equal,
            .minus_equal,
            .times_equal,
            .division_equal,
            .modulo_equal,
            .and_equal,
            .or_equal,
            .xor_equal,
            .shift_right_equal,
            .shift_left_equal,
            => {
                const expr = try p.expression() orelse {
                    try p.error_list.add(
                        p.peekToken(0).loc,
                        "expected expression, found '{s}'",
                        .{p.peekToken(0).tag.symbol()},
                        &.{},
                    );
                    return error.Parsing;
                };
                return try p.addNode(.{
                    .tag = .compound_assign,
                    .main_token = op_token,
                    .lhs = lhs,
                    .rhs = expr,
                });
            },
            else => {
                try p.error_list.add(
                    p.ast.getToken(op_token).loc,
                    "invalid assignment operator '{s}'",
                    .{p.ast.getToken(op_token).tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            },
        }
    }

    return null;
}

pub fn whileStatement(p: *Parser) !?Ast.Index {
    const main_token = p.eatToken(.k_while) orelse return null;
    const cond = try p.expression() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "expected condition expression, found '{s}'",
            .{p.peekToken(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
    const body = try p.expectBlock();
    return try p.addNode(.{
        .tag = .@"while",
        .main_token = main_token,
        .lhs = cond,
        .rhs = body,
    });
}

pub fn expectTypeSpecifier(p: *Parser) error{ OutOfMemory, Parsing }!Ast.Index {
    return try p.typeSpecifier() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "expected type sepecifier, found '{s}'",
            .{p.peekToken(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
}

pub fn typeSpecifier(p: *Parser) !?Ast.Index {
    if (p.peekToken(0).tag == .ident) {
        const main_token = p.advanceToken();
        return try p.addNode(.{ .tag = .user_type, .main_token = main_token });
    }
    return p.typeSpecifierWithoutIdent();
}

pub fn typeSpecifierWithoutIdent(p: *Parser) !?Ast.Index {
    if (p.isVectorPrefix() or p.isMatrixPrefix()) {
        const main_token = p.advanceToken();
        _ = try p.expectToken(.less_than);
        const elem_type = try p.expectTypeSpecifier();
        _ = try p.expectToken(.greater_than);
        return try p.addNode(.{
            .tag = if (p.isVectorPrefix()) .vector_type else .matrix_type,
            .main_token = main_token,
            .lhs = elem_type,
        });
    }

    const main_token = p.tok_i;
    switch (p.ast.getToken(main_token).tag) {
        .k_i32,
        .k_u32,
        .k_f32,
        .k_f16,
        .k_bool,
        => {
            _ = p.advanceToken();
            return try p.addNode(.{ .tag = .scalar_type, .main_token = main_token });
        },
        .k_sampler, .k_comparison_sampler => {
            _ = p.advanceToken();
            return try p.addNode(.{ .tag = .sampler_type, .main_token = main_token });
        },
        .k_atomic => {
            _ = p.advanceToken();
            _ = try p.expectToken(.less_than);
            const elem_type = try p.expectTypeSpecifier();
            _ = try p.expectToken(.greater_than);
            return try p.addNode(.{
                .tag = .atomic_type,
                .main_token = main_token,
                .lhs = elem_type,
            });
        },
        .k_array => {
            _ = p.advanceToken();
            _ = try p.expectToken(.less_than);
            const elem_type = try p.expectTypeSpecifier();
            var size = Ast.null_index;
            if (p.eatToken(.comma)) |_| {
                size = try p.elementCountExpr() orelse {
                    try p.error_list.add(
                        p.peekToken(0).loc,
                        "expected array size expression, found '{s}'",
                        .{p.peekToken(0).tag.symbol()},
                        &.{},
                    );
                    return error.Parsing;
                };
            }
            _ = try p.expectToken(.greater_than);
            return try p.addNode(.{
                .tag = .array_type,
                .main_token = main_token,
                .lhs = elem_type,
                .rhs = size,
            });
        },
        .k_ptr => {
            _ = p.advanceToken();
            _ = try p.expectToken(.less_than);

            const addr_space = try p.expectAddressSpace();
            _ = try p.expectToken(.comma);
            const elem_type = try p.expectTypeSpecifier();
            var access_mode = Ast.null_index;
            if (p.eatToken(.comma)) |_| {
                access_mode = try p.expectAccessMode();
            }
            _ = try p.expectToken(.greater_than);

            const extra = try p.addExtra(Ast.Node.PtrType{
                .addr_space = addr_space,
                .access_mode = access_mode,
            });
            return try p.addNode(.{
                .tag = .ptr_type,
                .main_token = main_token,
                .lhs = elem_type,
                .rhs = extra,
            });
        },
        else => return null,
    }
}

pub fn isVectorPrefix(p: *Parser) bool {
    return switch (p.peekToken(0).tag) {
        .k_vec2,
        .k_vec3,
        .k_vec4,
        => true,
        else => false,
    };
}

pub fn isMatrixPrefix(p: *Parser) bool {
    return switch (p.peekToken(0).tag) {
        .k_mat2x2,
        .k_mat2x3,
        .k_mat2x4,
        .k_mat3x2,
        .k_mat3x3,
        .k_mat3x4,
        .k_mat4x2,
        .k_mat4x3,
        .k_mat4x4,
        => true,
        else => false,
    };
}

pub fn expectAddressSpace(p: *Parser) !Ast.Index {
    const token = p.advanceToken();
    if (p.ast.getToken(token).tag == .ident) {
        const str = p.ast.getToken(token).loc.slice(p.ast.source);
        if (std.meta.stringToEnum(Ast.AddressSpace, str)) |_| return token;
    }

    try p.error_list.add(
        p.ast.getToken(token).loc,
        "unknown address space '{s}'",
        .{p.ast.getToken(token).loc.slice(p.ast.source)},
        comptime &.{comptimePrint("valid options are [{s}]", .{fieldNames(Ast.AddressSpace)})},
    );
    return error.Parsing;
}

pub fn expectAccessMode(p: *Parser) !Ast.Index {
    const token = p.advanceToken();
    if (p.ast.getToken(token).tag == .ident) {
        const str = p.ast.getToken(token).loc.slice(p.ast.source);
        if (std.meta.stringToEnum(Ast.AccessMode, str)) |_| return token;
    }

    try p.error_list.add(
        p.ast.getToken(token).loc,
        "unknown access mode '{s}'",
        .{p.ast.getToken(token).loc.slice(p.ast.source)},
        comptime &.{comptimePrint("valid options are [{s}]", .{fieldNames(Ast.AccessMode)})},
    );
    return error.Parsing;
}

pub fn expectParenExpr(p: *Parser) !Ast.Index {
    _ = try p.expectToken(.paren_left);
    const expr = try p.expression() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "unable to parse expression '{s}'",
            .{p.peekToken(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
    _ = try p.expectToken(.paren_right);
    return expr;
}

pub fn callExpr(p: *Parser) !?Ast.Index {
    const main_token = p.tok_i;
    var lhs = Ast.null_index;

    // function call
    if (p.peekToken(0).tag == .ident and p.peekToken(1).tag == .paren_left) {
        _ = p.advanceToken();
    }
    // without template args ('vec2', 'array', etc)
    else if (p.peekToken(1).tag != .less_than and
        (p.isVectorPrefix() or
        p.isMatrixPrefix() or
        p.peekToken(0).tag == .k_array))
    {
        _ = p.advanceToken();
    } else {
        // maybe with template args ('i32', 'vec2<f32>', 'array<i32>', etc)
        const type_node = try p.typeSpecifierWithoutIdent() orelse return null;
        const tag = p.ast.nodes.items(.tag)[type_node];
        switch (tag) {
            .scalar_type,
            .vector_type,
            .matrix_type,
            .array_type,
            => lhs = type_node,
            else => {
                try p.error_list.add(
                    p.ast.getToken(main_token).loc,
                    "type '{s}' can not be constructed",
                    .{p.ast.getToken(main_token).tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            },
        }
    }

    const rhs = try p.expectArgumentListExpr();
    return try p.addNode(.{
        .tag = .call,
        .main_token = main_token,
        .lhs = lhs,
        .rhs = rhs,
    });
}

pub fn expectArgumentListExpr(p: *Parser) !Ast.Index {
    _ = try p.expectToken(.paren_left);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const expr = try p.expression() orelse break;
        try p.scratch.append(p.allocator, expr);
        if (p.eatToken(.comma) == null) break;
    }

    _ = try p.expectToken(.paren_right);

    const list = p.scratch.items[scratch_top..];
    return p.listToSpan(list);
}

pub fn expression(p: *Parser) !?Ast.Index {
    const lhs_unary = try p.unaryExpr() orelse return null;
    if (try p.bitwiseExpr(lhs_unary)) |bitwise| return bitwise;
    const lhs = try p.expectRelationalExpr(lhs_unary);
    return try p.expectShortCircuitExpr(lhs);
}

pub fn lhsExpression(p: *Parser) !?Ast.Index {
    if (p.eatToken(.ident)) |ident_token| {
        return try p.componentOrSwizzleSpecifier(
            try p.addNode(.{ .tag = .ident_expr, .main_token = ident_token }),
        );
    }

    if (p.eatToken(.paren_left)) |_| {
        const expr = try p.lhsExpression() orelse {
            try p.error_list.add(
                p.peekToken(0).loc,
                "expected lhs expression, found '{s}'",
                .{p.peekToken(0).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
        _ = try p.expectToken(.paren_right);
        return try p.componentOrSwizzleSpecifier(expr);
    }

    if (p.eatToken(.star)) |star_token| {
        return try p.addNode(.{
            .tag = .deref,
            .main_token = star_token,
            .lhs = try p.lhsExpression() orelse {
                try p.error_list.add(
                    p.peekToken(0).loc,
                    "expected lhs expression, found '{s}'",
                    .{p.peekToken(0).tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            },
        });
    }

    if (p.eatToken(.@"and")) |addr_of_token| {
        return try p.addNode(.{
            .tag = .addr_of,
            .main_token = addr_of_token,
            .lhs = try p.lhsExpression() orelse {
                try p.error_list.add(
                    p.peekToken(0).loc,
                    "expected lhs expression, found '{s}'",
                    .{p.peekToken(0).tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            },
        });
    }

    return null;
}

pub fn singularExpr(p: *Parser) !?Ast.Index {
    const prefix = try p.primaryExpr() orelse return null;
    return try p.componentOrSwizzleSpecifier(prefix);
}

pub fn primaryExpr(p: *Parser) !?Ast.Index {
    const main_token = p.tok_i;
    if (try p.callExpr()) |call| return call;
    switch (p.ast.getToken(main_token).tag) {
        .k_true, .k_false => {
            _ = p.advanceToken();
            return try p.addNode(.{ .tag = .bool_literal, .main_token = main_token });
        },
        .number => {
            _ = p.advanceToken();
            return try p.addNode(.{ .tag = .number_literal, .main_token = main_token });
        },
        .k_bitcast => {
            _ = p.advanceToken();
            _ = try p.expectToken(.less_than);
            const dest_type = try p.expectTypeSpecifier();
            _ = try p.expectToken(.greater_than);
            const expr = try p.expectParenExpr();
            return try p.addNode(.{
                .tag = .bitcast,
                .main_token = main_token,
                .lhs = dest_type,
                .rhs = expr,
            });
        },
        .paren_left => return try p.expectParenExpr(),
        .ident => {
            _ = p.advanceToken();
            return try p.addNode(.{ .tag = .ident_expr, .main_token = main_token });
        },
        else => {
            return null;
        },
    }
}

pub fn elementCountExpr(p: *Parser) !?Ast.Index {
    const left = try p.unaryExpr() orelse return null;
    if (try p.bitwiseExpr(left)) |right| return right;
    return try p.expectMathExpr(left);
}

pub fn unaryExpr(p: *Parser) error{ OutOfMemory, Parsing }!?Ast.Index {
    const op_token = p.tok_i;
    const op: Ast.Node.Tag = switch (p.ast.getToken(op_token).tag) {
        .bang, .tilde => .not,
        .minus => .negate,
        .star => .deref,
        .@"and" => .addr_of,
        else => return p.singularExpr(),
    };
    _ = p.advanceToken();

    const expr = try p.unaryExpr() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "unable to parse right side of '{s}' expression",
            .{p.ast.getToken(op_token).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };

    return try p.addNode(.{
        .tag = op,
        .main_token = op_token,
        .lhs = expr,
    });
}

pub fn expectRelationalExpr(p: *Parser, lhs_unary: Ast.Index) !Ast.Index {
    const lhs = try p.expectShiftExpr(lhs_unary);
    const op_token = p.tok_i;
    const op: Ast.Node.Tag = switch (p.ast.getToken(op_token).tag) {
        .equal_equal => .equal,
        .not_equal => .not_equal,
        .less_than => .less,
        .less_than_equal => .less_equal,
        .greater_than => .greater,
        .greater_than_equal => .greater_equal,
        else => return lhs,
    };
    _ = p.advanceToken();

    const rhs_unary = try p.unaryExpr() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "unable to parse right side of '{s}' expression",
            .{p.ast.getToken(op_token).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
    const rhs = try p.expectShiftExpr(rhs_unary);
    return try p.addNode(.{
        .tag = op,
        .main_token = op_token,
        .lhs = lhs,
        .rhs = rhs,
    });
}

pub fn expectShortCircuitExpr(p: *Parser, lhs_relational: Ast.Index) !Ast.Index {
    var lhs = lhs_relational;

    const op_token = p.tok_i;
    const op: Ast.Node.Tag = switch (p.ast.getToken(op_token).tag) {
        .and_and => .circuit_and,
        .or_or => .circuit_or,
        else => return lhs,
    };

    while (p.peekToken(0).tag == p.ast.getToken(op_token).tag) {
        _ = p.advanceToken();

        const rhs_unary = try p.unaryExpr() orelse {
            try p.error_list.add(
                p.peekToken(0).loc,
                "unable to parse right side of '{s}' expression",
                .{p.ast.getToken(op_token).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
        const rhs = try p.expectRelationalExpr(rhs_unary);

        lhs = try p.addNode(.{
            .tag = op,
            .main_token = op_token,
            .lhs = lhs,
            .rhs = rhs,
        });
    }

    return lhs;
}

pub fn bitwiseExpr(p: *Parser, lhs: Ast.Index) !?Ast.Index {
    const op_token = p.tok_i;
    const op: Ast.Node.Tag = switch (p.ast.getToken(op_token).tag) {
        .@"and" => .binary_and,
        .@"or" => .binary_or,
        .xor => .binary_xor,
        else => return null,
    };
    _ = p.advanceToken();

    var lhs_result = lhs;
    while (true) {
        const rhs = try p.unaryExpr() orelse {
            try p.error_list.add(
                p.peekToken(0).loc,
                "unable to parse right side of '{s}' expression",
                .{p.ast.getToken(op_token).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };

        lhs_result = try p.addNode(.{
            .tag = op,
            .main_token = op_token,
            .lhs = lhs_result,
            .rhs = rhs,
        });

        if (p.peekToken(0).tag != p.ast.getToken(op_token).tag) return lhs_result;
    }
}

pub fn expectShiftExpr(p: *Parser, lhs: Ast.Index) !Ast.Index {
    const op_token = p.tok_i;
    const op: Ast.Node.Tag = switch (p.ast.getToken(op_token).tag) {
        .shift_left => .shift_left,
        .shift_right => .shift_right,
        else => return try p.expectMathExpr(lhs),
    };
    _ = p.advanceToken();

    const rhs = try p.unaryExpr() orelse {
        try p.error_list.add(
            p.peekToken(0).loc,
            "unable to parse right side of '{s}' expression",
            .{p.ast.getToken(op_token).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };

    return try p.addNode(.{
        .tag = op,
        .main_token = op_token,
        .lhs = lhs,
        .rhs = rhs,
    });
}

pub fn expectMathExpr(p: *Parser, left: Ast.Index) !Ast.Index {
    const right = try p.expectMultiplicativeExpr(left);
    return p.expectAdditiveExpr(right);
}

pub fn expectAdditiveExpr(p: *Parser, lhs_mul: Ast.Index) !Ast.Index {
    var lhs = lhs_mul;
    while (true) {
        const op_token = p.tok_i;
        const op: Ast.Node.Tag = switch (p.ast.getToken(op_token).tag) {
            .plus => .add,
            .minus => .sub,
            else => return lhs,
        };
        _ = p.advanceToken();
        const unary = try p.unaryExpr() orelse {
            try p.error_list.add(
                p.peekToken(0).loc,
                "unable to parse right side of '{s}' expression",
                .{p.ast.getToken(op_token).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
        const rhs = try p.expectMultiplicativeExpr(unary);
        lhs = try p.addNode(.{
            .tag = op,
            .main_token = op_token,
            .lhs = lhs,
            .rhs = rhs,
        });
    }
}

pub fn expectMultiplicativeExpr(p: *Parser, lhs_unary: Ast.Index) !Ast.Index {
    var lhs = lhs_unary;
    while (true) {
        const op_token = p.tok_i;
        const node_tag: Ast.Node.Tag = switch (p.peekToken(0).tag) {
            .star => .mul,
            .division => .div,
            .mod => .mod,
            else => return lhs,
        };
        _ = p.advanceToken();
        const rhs = try p.unaryExpr() orelse {
            try p.error_list.add(
                p.peekToken(0).loc,
                "unable to parse right side of '{s}' expression",
                .{p.peekToken(0).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
        lhs = try p.addNode(.{
            .tag = node_tag,
            .main_token = op_token,
            .lhs = lhs,
            .rhs = rhs,
        });
    }
}

pub fn componentOrSwizzleSpecifier(p: *Parser, prefix: Ast.Index) !Ast.Index {
    var prefix_result = prefix;
    while (true) {
        if (p.eatToken(.period)) |_| {
            const member_token = try p.expectToken(.ident);
            prefix_result = try p.addNode(.{
                .tag = .component_access,
                .main_token = member_token,
                .lhs = prefix_result,
            });
        } else if (p.eatToken(.bracket_left)) |bracket_left_token| {
            const index_expr = try p.expression() orelse {
                try p.error_list.add(
                    p.peekToken(0).loc,
                    "expected expression, but found '{s}'",
                    .{p.peekToken(0).tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            };
            _ = try p.expectToken(.bracket_right);
            prefix_result = try p.addNode(.{
                .tag = .index_access,
                .main_token = bracket_left_token,
                .lhs = prefix_result,
                .rhs = index_expr,
            });
        } else return prefix_result;
    }
}

fn findNextGlobal(p: *Parser) void {
    var level: Ast.Index = 0;
    while (true) {
        switch (p.peekToken(0).tag) {
            .k_fn,
            .k_var,
            .k_const,
            .k_override,
            .k_struct,
            .attr,
            => {
                if (level == 0) return;
            },
            .semicolon => {
                if (level == 0) {
                    _ = p.advanceToken();
                    return;
                }
            },
            .brace_left,
            .bracket_left,
            .paren_left,
            => {
                level += 1;
            },
            .brace_right => {
                if (level == 0) {
                    _ = p.advanceToken();
                    return;
                }
                level -= 1;
            },
            .bracket_right, .paren_right => {
                if (level != 0) level -= 1;
            },
            .eof => return,
            else => {},
        }
        _ = p.advanceToken();
    }
}

fn findNextStmt(p: *Parser) void {
    var level: Ast.Index = 0;
    while (true) {
        switch (p.peekToken(0).tag) {
            .semicolon => {
                if (level == 0) {
                    _ = p.advanceToken();
                    return;
                }
            },
            .brace_left => {
                level += 1;
            },
            .brace_right => {
                if (level == 0) {
                    _ = p.advanceToken();
                    return;
                }
                level -= 1;
            },
            .eof => return,
            else => {},
        }
        _ = p.advanceToken();
    }
}

fn listToSpan(p: *Parser, list: []const Ast.Index) !Ast.Index {
    try p.ast.extra.appendSlice(p.allocator, list);
    return p.addNode(.{
        .tag = .span,
        .main_token = undefined,
        .lhs = @intCast(Ast.Index, p.ast.extra.items.len - list.len),
        .rhs = @intCast(Ast.Index, p.ast.extra.items.len),
    });
}

pub fn addNode(p: *Parser, node: Ast.Node) error{OutOfMemory}!Ast.Index {
    const i = @intCast(Ast.Index, p.ast.nodes.len);
    try p.ast.nodes.append(p.allocator, node);
    return i;
}

fn addExtra(p: *Parser, extra: anytype) error{OutOfMemory}!Ast.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try p.ast.extra.ensureUnusedCapacity(p.allocator, fields.len);
    const result = @intCast(Ast.Index, p.ast.extra.items.len);
    inline for (fields) |field| {
        comptime std.debug.assert(field.type == Ast.Index or field.type == Ast.Index);
        p.ast.extra.appendAssumeCapacity(@field(extra, field.name));
    }
    return result;
}

pub fn peekToken(self: Parser, offset: Ast.Index) Token {
    return self.ast.getToken(self.tok_i + offset);
}

pub fn advanceToken(self: *Parser) Ast.Index {
    const prev = self.tok_i;
    self.tok_i = std.math.min(prev +| 1, self.ast.tokens.items.len);
    return prev;
}

pub fn eatToken(p: *Parser, tag: Token.Tag) ?Ast.Index {
    return if (p.peekToken(0).tag == tag) p.advanceToken() else null;
}

pub fn expectToken(p: *Parser, tag: Token.Tag) !Ast.Index {
    const token = p.advanceToken();
    if (p.ast.getToken(token).tag == tag) return token;

    try p.error_list.add(
        p.ast.getToken(token).loc,
        "expected '{s}', but found '{s}'",
        .{ tag.symbol(), p.ast.getToken(token).tag.symbol() },
        &.{},
    );
    return error.Parsing;
}
