//! Based on <SPEC_COMMIT>
const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");
const TokenList = @import("TokenList.zig");
const comptimePrint = std.fmt.comptimePrint;
const fieldNames = std.meta.fieldNames;
const null_index = Ast.null_index;
const Parser = @This();

allocator: std.mem.Allocator,
source: [:0]const u8,
ast: Ast,
error_file: std.fs.File,
scratch: std.ArrayListUnmanaged(Ast.Node.Index) = .{},
failed: bool = false,

fn findNextGlobal(p: *Parser) void {
    var level: TokenList.Index = 0;
    while (true) {
        switch (p.ast.tokens.peek(0).tag) {
            .keyword_fn,
            .keyword_var,
            .keyword_const,
            .keyword_override,
            .keyword_struct,
            .attr,
            => {
                if (level == 0) return;
            },
            .semicolon => {
                if (level == 0) {
                    _ = p.ast.tokens.advance();
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
                    _ = p.ast.tokens.advance();
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
        _ = p.ast.tokens.advance();
    }
}

fn findNextStmt(p: *Parser) void {
    var level: TokenList.Index = 0;
    while (true) {
        switch (p.ast.tokens.peek(0).tag) {
            .semicolon => {
                if (level == 0) {
                    _ = p.ast.tokens.advance();
                    return;
                }
            },
            .brace_left => {
                level += 1;
            },
            .brace_right => {
                if (level == 0) {
                    _ = p.ast.tokens.advance();
                    return;
                }
                level -= 1;
            },
            .eof => return,
            else => {},
        }
        _ = p.ast.tokens.advance();
    }
}

pub fn expectGlobalDeclRecoverable(p: *Parser) !?Ast.Node.Index {
    return p.expectGlobalDecl() catch |err| switch (err) {
        error.Parsing => {
            p.findNextGlobal();
            return null;
        },
        else => return err,
    };
}

pub fn expectGlobalDecl(p: *Parser) !Ast.Node.Index {
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

    p.addError(
        p.ast.tokens.peek(0).loc,
        "expected global declaration, found '{s}'",
        .{p.ast.tokens.peek(0).tag.symbol()},
        &.{},
    );
    return error.Parsing;
}

pub fn attributeList(p: *Parser) !?Ast.Node.Index {
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

pub fn attribute(p: *Parser) !?Ast.Node.Index {
    const attr_token = p.eatToken(.attr) orelse return null;
    const ident_tok = try p.expectToken(.ident);
    const str = p.ast.tokens.get(ident_tok).loc.asStr(p.source);
    const tag = std.meta.stringToEnum(Ast.Attribute, str) orelse {
        p.addError(
            p.ast.tokens.get(ident_tok).loc,
            "unknown attribute '{s}'",
            .{p.ast.tokens.get(ident_tok).loc.asStr(p.source)},
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
                    p.addError(
                        p.ast.tokens.peek(0).loc,
                        "expected expression, but found '{s}'",
                        .{p.ast.tokens.peek(0).tag.symbol()},
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
            node.lhs = try p.expression() orelse {
                p.addError(p.ast.tokens.peek(0).loc, "expected workgroup_size x parameter", .{}, &.{});
                return error.Parsing;
            };

            if (p.eatToken(.comma) != null and p.ast.tokens.peek(0).tag != .paren_right) {
                var workgroup_size = Ast.Node.WorkgroupSize{
                    .y = try p.expression() orelse {
                        p.addError(p.ast.tokens.peek(0).loc, "expected workgroup_size y parameter", .{}, &.{});
                        return error.Parsing;
                    },
                };

                if (p.eatToken(.comma) != null and p.ast.tokens.peek(0).tag != .paren_right) {
                    workgroup_size.z = try p.expression() orelse {
                        p.addError(p.ast.tokens.peek(0).loc, "expected workgroup_size z parameter", .{}, &.{});
                        return error.Parsing;
                    };

                    _ = p.eatToken(.comma);
                }

                node.rhs = try p.addExtra(workgroup_size);
            }

            _ = try p.expectToken(.paren_right);
        },
        .interpolate => {
            _ = try p.expectToken(.paren_left);

            node.tag = .attr_interpolate;
            node.lhs = try p.expectInterpolationType();

            if (p.eatToken(.comma) != null and p.ast.tokens.peek(0).tag != .paren_right) {
                node.rhs = try p.expectInterpolationSample();
                _ = p.eatToken(.comma);
                _ = try p.expectToken(.paren_right);
            }

            _ = try p.expectToken(.paren_right);
        },
    }

    return try p.addNode(node);
}

pub fn expectBuiltinValue(p: *Parser) !TokenList.Index {
    const token = p.ast.tokens.advance();
    if (p.ast.tokens.get(token).tag == .ident) {
        const str = p.ast.tokens.get(token).loc.asStr(p.source);
        if (std.meta.stringToEnum(Ast.BuiltinValue, str)) |_| return token;
    }

    p.addError(
        p.ast.tokens.get(token).loc,
        "unknown builtin value name '{s}'",
        .{p.ast.tokens.get(token).loc.asStr(p.source)},
        comptime &.{comptimePrint("valid options are [{s}]", .{fieldNames(Ast.BuiltinValue)})},
    );
    return error.Parsing;
}

pub fn expectInterpolationType(p: *Parser) !TokenList.Index {
    const token = p.ast.tokens.advance();
    if (p.ast.tokens.get(token).tag == .ident) {
        const str = p.ast.tokens.get(token).loc.asStr(p.source);
        if (std.meta.stringToEnum(Ast.InterpolationType, str)) |_| return token;
    }

    p.addError(
        p.ast.tokens.get(token).loc,
        "unknown interpolation type name '{s}'",
        .{p.ast.tokens.get(token).loc.asStr(p.source)},
        comptime &.{comptimePrint("valid options are [{s}]", .{fieldNames(Ast.InterpolationType)})},
    );
    return error.Parsing;
}

pub fn expectInterpolationSample(p: *Parser) !Ast.Node.Index {
    const token = p.ast.tokens.advance();
    if (p.ast.tokens.get(token).tag == .ident) {
        const str = p.ast.tokens.get(token).loc.asStr(p.source);
        if (std.meta.stringToEnum(Ast.InterpolationSample, str)) |_| return token;
    }

    p.addError(
        p.ast.tokens.get(token).loc,
        "unknown interpolation sample name '{s}'",
        .{p.ast.tokens.get(token).loc.asStr(p.source)},
        comptime &.{comptimePrint("valid options are [{s}]", .{fieldNames(Ast.InterpolationSample)})},
    );
    return error.Parsing;
}

pub fn globalVarDecl(p: *Parser, attrs: ?Ast.Node.Index) !?Ast.Node.Index {
    const var_token = p.eatToken(.keyword_var) orelse return null;

    // qualifier
    var addr_space = null_index;
    var access_mode = null_index;
    if (p.eatToken(.less_than)) |_| {
        addr_space = try p.expectAddressSpace();
        access_mode = if (p.eatToken(.comma)) |_|
            try p.expectAccessMode()
        else
            null_index;
        _ = try p.expectToken(.greater_than);
    }

    // name, type
    _ = try p.expectToken(.ident);
    var var_type = null_index;
    if (p.eatToken(.colon)) |_| {
        var_type = try p.expectTypeSpecifier();
    }

    var initializer = null_index;
    if (p.eatToken(.equal)) |_| {
        initializer = try p.expression() orelse {
            p.addError(
                p.ast.tokens.peek(0).loc,
                "expected initializer expression, found '{s}'",
                .{p.ast.tokens.peek(0).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
    }

    const extra = try p.addExtra(Ast.Node.GlobalVarDecl{
        .attrs = attrs orelse null_index,
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

pub fn globalConstDecl(p: *Parser) !?Ast.Node.Index {
    const const_token = p.eatToken(.keyword_const) orelse return null;

    _ = try p.expectToken(.ident);
    var const_type = null_index;
    if (p.eatToken(.colon)) |_| {
        const_type = try p.expectTypeSpecifier();
    }

    _ = try p.expectToken(.equal);
    const initializer = try p.expression() orelse {
        p.addError(
            p.ast.tokens.peek(0).loc,
            "expected initializer expression, found '{s}'",
            .{p.ast.tokens.peek(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };

    return try p.addNode(.{
        .tag = .global_variable,
        .main_token = const_token,
        .lhs = const_type,
        .rhs = initializer,
    });
}

pub fn globalOverrideDecl(p: *Parser, attrs: ?Ast.Node.Index) !?Ast.Node.Index {
    const override_token = p.eatToken(.keyword_override) orelse return null;

    // name, type
    _ = try p.expectToken(.ident);
    var override_type = null_index;
    if (p.eatToken(.colon)) |_| {
        override_type = try p.expectTypeSpecifier();
    }

    var initializer = null_index;
    if (p.eatToken(.equal)) |_| {
        initializer = try p.expression() orelse {
            p.addError(
                p.ast.tokens.peek(0).loc,
                "expected initializer expression, found '{s}'",
                .{p.ast.tokens.peek(0).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
    }

    const extra = try p.addExtra(Ast.Node.GlobalOverrideDecl{
        .attrs = attrs orelse null_index,
        .type = override_type,
    });
    return try p.addNode(.{
        .tag = .global_variable,
        .main_token = override_token,
        .lhs = extra,
        .rhs = initializer,
    });
}

pub fn typeAliasDecl(p: *Parser) !?Ast.Node.Index {
    const type_token = p.eatToken(.keyword_type) orelse return null;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.equal);
    const value = try p.expectTypeSpecifier();
    return try p.addNode(.{
        .tag = .type_alias,
        .main_token = type_token,
        .lhs = value,
    });
}

pub fn structDecl(p: *Parser) !?Ast.Node.Index {
    const main_token = p.eatToken(.keyword_struct) orelse return null;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.brace_left);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const attrs = try p.attributeList();
        const member = try p.structMember(attrs) orelse {
            if (attrs != null) {
                p.addError(
                    p.ast.tokens.peek(0).loc,
                    "expected struct member, found '{s}'",
                    .{p.ast.tokens.peek(0).tag.symbol()},
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

pub fn structMember(p: *Parser, attrs: ?Ast.Node.Index) !?Ast.Node.Index {
    const name_token = p.eatToken(.ident) orelse return null;
    _ = try p.expectToken(.colon);
    const member_type = try p.expectTypeSpecifier();
    return try p.addNode(.{
        .tag = .struct_member,
        .main_token = name_token,
        .lhs = attrs orelse null_index,
        .rhs = member_type,
    });
}

pub fn constAssert(p: *Parser) !?Ast.Node.Index {
    const main_token = p.eatToken(.keyword_const_assert) orelse return null;
    const expr = try p.expression() orelse {
        p.addError(
            p.ast.tokens.peek(0).loc,
            "expected expression, found '{s}'",
            .{p.ast.tokens.peek(0).tag.symbol()},
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

pub fn functionDecl(p: *Parser, attrs: ?Ast.Node.Index) !?Ast.Node.Index {
    const fn_token = p.eatToken(.keyword_fn) orelse return null;
    _ = try p.expectToken(.ident);

    _ = try p.expectToken(.paren_left);
    const params = try p.expectParameterList();
    _ = try p.expectToken(.paren_right);

    var result_attrs = null_index;
    var result_type = null_index;
    if (p.eatToken(.arrow)) |_| {
        result_attrs = try p.attributeList() orelse null_index;
        result_type = try p.expectTypeSpecifier();
    }

    const body = try p.block() orelse {
        p.addError(
            p.ast.tokens.peek(0).loc,
            "expected function body, found '{s}'",
            .{p.ast.tokens.peek(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };

    const fn_proto = try p.addExtra(Ast.Node.FnProto{
        .attrs = attrs orelse null_index,
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

/// ParameterList : Parameter (COMMA Param)* COMMA?
pub fn expectParameterList(p: *Parser) !Ast.Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const attrs = try p.attributeList();
        const param = try p.parameter(attrs) orelse {
            if (attrs != null) {
                p.addError(
                    p.ast.tokens.peek(0).loc,
                    "expected function parameter, found '{s}'",
                    .{p.ast.tokens.peek(0).tag.symbol()},
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

pub fn parameter(p: *Parser, attrs: ?Ast.Node.Index) !?Ast.Node.Index {
    const main_token = p.eatToken(.ident) orelse return null;
    _ = try p.expectToken(.colon);
    const param_type = try p.expectTypeSpecifier();
    return try p.addNode(.{
        .tag = .fn_param,
        .main_token = main_token,
        .lhs = attrs orelse null_index,
        .rhs = param_type,
    });
}

pub fn block(p: *Parser) error{ OutOfMemory, Parsing }!?Ast.Node.Index {
    _ = p.eatToken(.brace_left) orelse return null;

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    var failed = false;
    while (true) {
        const stmt = try p.statementRecoverable() orelse {
            if (p.ast.tokens.peek(0).tag == .brace_right) break;
            failed = true;
            p.addError(
                p.ast.tokens.peek(0).loc,
                "expected statement, found '{s}'",
                .{p.ast.tokens.peek(0).tag.symbol()},
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

pub fn expectBlock(p: *Parser) error{ OutOfMemory, Parsing }!Ast.Node.Index {
    return try p.block() orelse {
        p.addError(
            p.ast.tokens.peek(0).loc,
            "expected block statement, found '{s}'",
            .{p.ast.tokens.peek(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
}

/// Statement
///   | IfStatement                            TODO
///   | SwitchStatement                        TODO
///   | ForStatement                           TODO
///   | WhileStatement                         TODO
///   | FuncCallStatement         SEMICOLON    TODO
///   | VariableStatement         SEMICOLON    TODO
///   | BreakStatement            SEMICOLON
///   | ContinueStatement         SEMICOLON
///   | VariableUpdatingStatement SEMICOLON    TODO
///   | ConstAssertStatement      SEMICOLON
///
/// for simplicity and better error messages,
/// we are putting all statements here
pub fn statement(p: *Parser) !?Ast.Node.Index {
    while (p.eatToken(.semicolon)) |_| {}

    if (try p.returnStatement() orelse
        try p.discardStatement() orelse
        try p.breakIfStatement() orelse
        try p.breakStatement() orelse
        try p.continueStatement() orelse
        try p.constAssert()) |node|
    {
        _ = try p.expectToken(.semicolon);
        return node;
    }

    if (try p.loopStatement() orelse
        try p.continuingStatement() orelse
        try p.block()) |node|
    {
        return node;
    }

    return null;
}

pub fn statementRecoverable(p: *Parser) !?Ast.Node.Index {
    while (true) {
        return p.statement() catch |err| switch (err) {
            error.Parsing => {
                p.findNextStmt();
                switch (p.ast.tokens.peek(0).tag) {
                    .brace_right => return null,
                    .eof => return error.Parsing,
                    else => continue,
                }
            },
            else => return err,
        };
    }
}

pub fn returnStatement(p: *Parser) !?Ast.Node.Index {
    const main_token = p.eatToken(.keyword_return) orelse return null;
    const expr = try p.expression() orelse Ast.null_index;
    return try p.addNode(.{
        .tag = .return_statement,
        .main_token = main_token,
        .lhs = expr,
    });
}

pub fn discardStatement(p: *Parser) !?Ast.Node.Index {
    const main_token = p.eatToken(.keyword_discard) orelse return null;
    return try p.addNode(.{ .tag = .discard_statement, .main_token = main_token });
}

pub fn breakStatement(p: *Parser) !?Ast.Node.Index {
    const main_token = p.eatToken(.keyword_break) orelse return null;
    return try p.addNode(.{ .tag = .break_statement, .main_token = main_token });
}

pub fn continueStatement(p: *Parser) !?Ast.Node.Index {
    const main_token = p.eatToken(.keyword_continue) orelse return null;
    return try p.addNode(.{ .tag = .continue_statement, .main_token = main_token });
}

pub fn loopStatement(p: *Parser) !?Ast.Node.Index {
    const main_token = p.eatToken(.keyword_loop) orelse return null;
    const body = try p.expectBlock();
    return try p.addNode(.{
        .tag = .loop_statement,
        .main_token = main_token,
        .lhs = body,
    });
}

pub fn continuingStatement(p: *Parser) !?Ast.Node.Index {
    const main_token = p.eatToken(.keyword_continuing) orelse return null;
    const body = try p.expectBlock();
    return try p.addNode(.{
        .tag = .continuing_statement,
        .main_token = main_token,
        .lhs = body,
    });
}

pub fn breakIfStatement(p: *Parser) !?Ast.Node.Index {
    if (p.ast.tokens.peek(0).tag == .keyword_break and
        p.ast.tokens.peek(1).tag == .keyword_if)
    {
        const main_token = p.ast.tokens.advance();
        _ = p.ast.tokens.advance();
        const cond = try p.expression() orelse {
            p.addError(
                p.ast.tokens.peek(0).loc,
                "expected condition expression, found '{s}'",
                .{p.ast.tokens.peek(0).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
        return try p.addNode(.{
            .tag = .break_if_statement,
            .main_token = main_token,
            .lhs = cond,
        });
    }
    return null;
}

/// IfStatement : IF EXpr Block (ELSE IF Expr Block)* ELSE Block?
// pub fn ifStatement(p: *Parser) !?void {
//     if (p.eatToken(.keyword_if) == null) return null;

//     var stmts = std.ArrayList(Ast.Statement).init(p.allocator);
//     defer stmts.deinit();
//     while (true) {
//         const cond = p.expression() orelse {
//             p.addError(
//                 p.ast.tokens.peek(0).loc,
//                 "expected condition expression, found '{s}'",
//                 .{p.ast.tokens.peek(0).tag.symbol()},
//                 &.{},
//             );
//             return error.Parsing;
//         };
//         const payload = try p.block() orelse {
//             p.addError(
//                 p.ast.tokens.peek(0).loc,
//                 "expected payload block, found '{s}'",
//                 .{p.ast.tokens.peek(0).tag.symbol()},
//                 &.{},
//             );
//             return error.Parsing;
//         };
//         try stmts.append(.{ .cond = cond, .payload = payload });

//         if (p.eatToken(.@"else")) |_| {}
//     }
// }

// /// VariableStatement
// ///   : VariableDecl                          TODO
// ///   | VariableDecl       EQUAL Expr         TODO
// ///   | LET   OptionalType EQUAL Expr
// ///   | CONST OptionalType EQUAL Expr
// pub fn varStatement(p: *Parser) !Ast.Variable {
//     switch (p.ast.tokens.peek(0).tag) {
//         .keyword_let, .keyword_const => {
//             _ = p.ast.tokens.advance();

//             // const opt_type_ident = try p.expectOptionalyTypedIdent();
//             // _ = try p.expectToken(.equal);
//             // const value = p.expression() catch |err| {
//             //     if (err == error.Parsing) {
//             //         p.addError(
//             //             p.ast.tokens.peek(0).loc,
//             //             "expected initializer expression, found '{s}'",
//             //             .{p.ast.tokens.peek(0).tag.symbol()},
//             //             &.{},
//             //         );
//             //     }
//             //     return err;
//             // };
//             // _ = try p.expectToken(.semicolon);

//             // const scope: Ast.Variable.Scope = switch (tag) {
//             //     .keyword_let => .function,
//             //     .keyword_const => .module_or_function,
//             //     else => unreachable,
//             // };

//             // return .{
//             //     .name = opt_type_ident.name,
//             //     .type = opt_type_ident.type,
//             //     .value = value,
//             // };
//             return error.Parsing;
//         },
//         else => return error.Parsing,
//     }
// }

pub fn expectTypeSpecifier(p: *Parser) error{ OutOfMemory, Parsing }!Ast.Node.Index {
    return try p.typeSpecifier() orelse {
        p.addError(
            p.ast.tokens.peek(0).loc,
            "expected type sepecifier, found '{s}'",
            .{p.ast.tokens.peek(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
}

pub fn typeSpecifier(p: *Parser) !?Ast.Node.Index {
    if (p.ast.tokens.peek(0).tag == .ident) {
        const main_token = p.ast.tokens.advance();
        return try p.addNode(.{ .tag = .user_type, .main_token = main_token });
    }
    return p.typeSpecifierWithoutIdent();
}

pub fn typeSpecifierWithoutIdent(p: *Parser) !?Ast.Node.Index {
    if (p.isVectorPrefix() or p.isMatrixPrefix()) {
        const main_token = p.ast.tokens.advance();
        _ = try p.expectToken(.less_than);
        const elem_type = try p.expectTypeSpecifier();
        _ = try p.expectToken(.greater_than);
        return try p.addNode(.{
            .tag = if (p.isVectorPrefix()) .vector_type else .matrix_type,
            .main_token = main_token,
            .lhs = elem_type,
        });
    }

    const main_token = p.ast.tokens.index;
    switch (p.ast.tokens.get(main_token).tag) {
        .keyword_i32,
        .keyword_u32,
        .keyword_f32,
        .keyword_f16,
        .keyword_bool,
        => {
            _ = p.ast.tokens.advance();
            return try p.addNode(.{ .tag = .scalar_type, .main_token = main_token });
        },
        .keyword_sampler, .keyword_comparison_sampler => {
            _ = p.ast.tokens.advance();
            return try p.addNode(.{ .tag = .sampler_type, .main_token = main_token });
        },
        .keyword_atomic => {
            _ = p.ast.tokens.advance();
            _ = try p.expectToken(.less_than);
            const elem_type = try p.expectTypeSpecifier();
            _ = try p.expectToken(.greater_than);
            return try p.addNode(.{
                .tag = .atomic_type,
                .main_token = main_token,
                .lhs = elem_type,
            });
        },
        .keyword_array => {
            _ = p.ast.tokens.advance();
            _ = try p.expectToken(.less_than);
            const elem_type = try p.expectTypeSpecifier();
            var size = null_index;
            if (p.eatToken(.comma)) |_| {
                size = try p.elementCountExpr() orelse {
                    p.addError(
                        p.ast.tokens.peek(0).loc,
                        "expected array size expression, found '{s}'",
                        .{p.ast.tokens.peek(0).tag.symbol()},
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
        .keyword_ptr => {
            _ = p.ast.tokens.advance();
            _ = try p.expectToken(.less_than);

            const addr_space = try p.expectAddressSpace();
            _ = try p.expectToken(.comma);
            const elem_type = try p.expectTypeSpecifier();
            var access_mode = null_index;
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
    return switch (p.ast.tokens.peek(0).tag) {
        .keyword_vec2,
        .keyword_vec3,
        .keyword_vec4,
        => true,
        else => false,
    };
}

pub fn isMatrixPrefix(p: *Parser) bool {
    return switch (p.ast.tokens.peek(0).tag) {
        .keyword_mat2x2,
        .keyword_mat2x3,
        .keyword_mat2x4,
        .keyword_mat3x2,
        .keyword_mat3x3,
        .keyword_mat3x4,
        .keyword_mat4x2,
        .keyword_mat4x3,
        .keyword_mat4x4,
        => true,
        else => false,
    };
}

pub fn expectAddressSpace(p: *Parser) !TokenList.Index {
    const token = p.ast.tokens.advance();
    if (p.ast.tokens.get(token).tag == .ident) {
        const str = p.ast.tokens.get(token).loc.asStr(p.source);
        if (std.meta.stringToEnum(Ast.AddressSpace, str)) |_| return token;
    }

    p.addError(
        p.ast.tokens.get(token).loc,
        "unknown address space '{s}'",
        .{p.ast.tokens.get(token).loc.asStr(p.source)},
        comptime &.{comptimePrint("valid options are [{s}]", .{fieldNames(Ast.AddressSpace)})},
    );
    return error.Parsing;
}

pub fn expectAccessMode(p: *Parser) !TokenList.Index {
    const token = p.ast.tokens.advance();
    if (p.ast.tokens.get(token).tag == .ident) {
        const str = p.ast.tokens.get(token).loc.asStr(p.source);
        if (std.meta.stringToEnum(Ast.AccessMode, str)) |_| return token;
    }

    p.addError(
        p.ast.tokens.get(token).loc,
        "unknown access mode '{s}'",
        .{p.ast.tokens.get(token).loc.asStr(p.source)},
        comptime &.{comptimePrint("valid options are [{s}]", .{fieldNames(Ast.AccessMode)})},
    );
    return error.Parsing;
}

pub fn primaryExpr(p: *Parser) !?Ast.Node.Index {
    const main_token = p.ast.tokens.index;
    if (try p.callExpr()) |call| return call;
    switch (p.ast.tokens.get(main_token).tag) {
        .keyword_true, .keyword_false => {
            _ = p.ast.tokens.advance();
            return try p.addNode(.{ .tag = .bool_literal, .main_token = main_token });
        },
        .number => {
            _ = p.ast.tokens.advance();
            return try p.addNode(.{ .tag = .number_literal, .main_token = main_token });
        },
        .keyword_bitcast => {
            _ = p.ast.tokens.advance();
            _ = try p.expectToken(.less_than);
            const dest_type = try p.expectTypeSpecifier();
            _ = try p.expectToken(.greater_than);
            const expr = try p.expectParenExpr();
            return try p.addNode(.{
                .tag = .bitcast_expr,
                .main_token = main_token,
                .lhs = dest_type,
                .rhs = expr,
            });
        },
        .paren_left => return try p.expectParenExpr(),
        .ident => {
            _ = p.ast.tokens.advance();
            return try p.addNode(.{ .tag = .ident_expr, .main_token = main_token });
        },
        else => {
            return null;
        },
    }
}

pub fn expectParenExpr(p: *Parser) !Ast.Node.Index {
    _ = try p.expectToken(.paren_left);
    const expr = try p.expression() orelse {
        p.addError(
            p.ast.tokens.peek(0).loc,
            "unable to parse expression '{s}'",
            .{p.ast.tokens.peek(0).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
    _ = try p.expectToken(.paren_right);
    return expr;
}

pub fn callExpr(p: *Parser) !?Ast.Node.Index {
    const main_token = p.ast.tokens.index;
    var lhs = null_index;

    // function call
    if (p.ast.tokens.peek(0).tag == .ident and p.ast.tokens.peek(1).tag == .paren_left) {
        _ = p.ast.tokens.advance();
    }
    // without template args ('vec2', 'array', etc)
    else if (p.ast.tokens.peek(1).tag != .less_than and
        (p.isVectorPrefix() or
        p.isMatrixPrefix() or
        p.ast.tokens.peek(0).tag == .keyword_array))
    {
        _ = p.ast.tokens.advance();
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
                p.addError(
                    p.ast.tokens.get(main_token).loc,
                    "type '{s}' can not be constructed",
                    .{p.ast.tokens.get(main_token).tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            },
        }
    }

    const rhs = try p.expectArgumentExprList();
    return try p.addNode(.{
        .tag = .call_expr,
        .main_token = main_token,
        .lhs = lhs,
        .rhs = rhs,
    });
}

pub fn expectArgumentExprList(p: *Parser) !Ast.Node.Index {
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

pub fn elementCountExpr(p: *Parser) !?Ast.Node.Index {
    const left = try p.unaryExpr() orelse return null;
    if (try p.bitwiseExpr(left)) |right| return right;
    return try p.expectMathExpr(left);
}

pub fn unaryExpr(p: *Parser) error{ OutOfMemory, Parsing }!?Ast.Node.Index {
    const op_token = p.ast.tokens.index;
    const op: Ast.Node.Tag = switch (p.ast.tokens.get(op_token).tag) {
        .bang, .tilde => .not,
        .minus => .negate,
        .star => .deref,
        .@"and" => .addr_of,
        else => return p.singularExpr(),
    };
    _ = p.ast.tokens.advance();

    const expr = try p.unaryExpr() orelse {
        p.addError(
            p.ast.tokens.peek(0).loc,
            "unable to parse right side of '{s}' expression",
            .{p.ast.tokens.get(op_token).tag.symbol()},
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

pub fn singularExpr(p: *Parser) !?Ast.Node.Index {
    return p.primaryExpr();
    // TODO: component_or_swizzle_specifier
}

pub fn expectMultiplicativeExpr(p: *Parser, lhs_unary: Ast.Node.Index) !Ast.Node.Index {
    var lhs = lhs_unary;
    while (true) {
        const op_token = p.ast.tokens.index;
        const node_tag: Ast.Node.Tag = switch (p.ast.tokens.peek(0).tag) {
            .star => .mul,
            .division => .div,
            .mod => .mod,
            else => return lhs,
        };
        _ = p.ast.tokens.advance();
        const rhs = try p.unaryExpr() orelse {
            p.addError(
                p.ast.tokens.peek(0).loc,
                "unable to parse right side of '{s}' expression",
                .{p.ast.tokens.peek(0).tag.symbol()},
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

/// TODO: ComponentOrSwizzleSpecifier
pub fn expectAdditiveExpr(p: *Parser, lhs_mul: Ast.Node.Index) !Ast.Node.Index {
    var lhs = lhs_mul;
    while (true) {
        const op_token = p.ast.tokens.index;
        const op: Ast.Node.Tag = switch (p.ast.tokens.get(op_token).tag) {
            .plus => .add,
            .minus => .sub,
            else => return lhs,
        };
        _ = p.ast.tokens.advance();
        const unary = try p.unaryExpr() orelse {
            p.addError(
                p.ast.tokens.peek(0).loc,
                "unable to parse right side of '{s}' expression",
                .{p.ast.tokens.get(op_token).tag.symbol()},
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

pub fn expectMathExpr(p: *Parser, left: Ast.Node.Index) !Ast.Node.Index {
    const right = try p.expectMultiplicativeExpr(left);
    return p.expectAdditiveExpr(right);
}

pub fn expectShiftExpr(p: *Parser, lhs: Ast.Node.Index) !Ast.Node.Index {
    const op_token = p.ast.tokens.index;
    const op: Ast.Node.Tag = switch (p.ast.tokens.get(op_token).tag) {
        .shift_left => .shift_left,
        .shift_right => .shift_right,
        else => return try p.expectMathExpr(lhs),
    };
    _ = p.ast.tokens.advance();

    const rhs = try p.unaryExpr() orelse {
        p.addError(
            p.ast.tokens.peek(0).loc,
            "unable to parse right side of '{s}' expression",
            .{p.ast.tokens.get(op_token).tag.symbol()},
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

pub fn expectRelationalExpr(p: *Parser, lhs_unary: Ast.Node.Index) !Ast.Node.Index {
    const lhs = try p.expectShiftExpr(lhs_unary);
    const op_token = p.ast.tokens.index;
    const op: Ast.Node.Tag = switch (p.ast.tokens.get(op_token).tag) {
        .equal_equal => .equal,
        .not_equal => .not_equal,
        .less_than => .less,
        .less_than_equal => .less_equal,
        .greater_than => .greater,
        .greater_than_equal => .greater_equal,
        else => return lhs,
    };
    _ = p.ast.tokens.advance();

    const rhs_unary = try p.unaryExpr() orelse {
        p.addError(
            p.ast.tokens.peek(0).loc,
            "unable to parse right side of '{s}' expression",
            .{p.ast.tokens.get(op_token).tag.symbol()},
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

pub fn bitwiseExpr(p: *Parser, lhs: Ast.Node.Index) !?Ast.Node.Index {
    const op_token = p.ast.tokens.index;
    const op: Ast.Node.Tag = switch (p.ast.tokens.get(op_token).tag) {
        .@"and" => .binary_and,
        .@"or" => .binary_or,
        .xor => .binary_xor,
        else => return null,
    };
    _ = p.ast.tokens.advance();

    var lhs_result = lhs;
    while (true) {
        const rhs = try p.unaryExpr() orelse {
            p.addError(
                p.ast.tokens.peek(0).loc,
                "unable to parse right side of '{s}' expression",
                .{p.ast.tokens.get(op_token).tag.symbol()},
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

        if (p.ast.tokens.peek(0).tag != p.ast.tokens.get(op_token).tag) return lhs_result;
    }
}

pub fn expectShortCircuitExpr(p: *Parser, lhs_relational: Ast.Node.Index) !Ast.Node.Index {
    var lhs = lhs_relational;

    const op_token = p.ast.tokens.index;
    const op: Ast.Node.Tag = switch (p.ast.tokens.get(op_token).tag) {
        .and_and => .circuit_and,
        .or_or => .circuit_or,
        else => return lhs,
    };

    while (p.ast.tokens.peek(0).tag == p.ast.tokens.get(op_token).tag) {
        _ = p.ast.tokens.advance();

        const rhs_unary = try p.unaryExpr() orelse {
            p.addError(
                p.ast.tokens.peek(0).loc,
                "unable to parse right side of '{s}' expression",
                .{p.ast.tokens.get(op_token).tag.symbol()},
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

pub fn expression(p: *Parser) !?Ast.Node.Index {
    const lhs_unary = try p.unaryExpr() orelse return null;
    if (try p.bitwiseExpr(lhs_unary)) |bitwise| return bitwise;
    const lhs = try p.expectRelationalExpr(lhs_unary);
    return try p.expectShortCircuitExpr(lhs);
}

pub fn expectToken(p: *Parser, tag: Token.Tag) !TokenList.Index {
    const token = p.ast.tokens.advance();
    if (p.ast.tokens.get(token).tag == tag) return token;

    p.addError(
        p.ast.tokens.get(token).loc,
        "expected '{s}', but found '{s}'",
        .{ tag.symbol(), p.ast.tokens.get(token).tag.symbol() },
        &.{},
    );
    return error.Parsing;
}

pub fn eatToken(p: *Parser, tag: Token.Tag) ?TokenList.Index {
    return if (p.ast.tokens.peek(0).tag == tag) p.ast.tokens.advance() else null;
}

pub fn addNode(p: *Parser, node: Ast.Node) error{OutOfMemory}!Ast.Node.Index {
    const i = @intCast(Ast.Node.Index, p.ast.nodes.len);
    try p.ast.nodes.append(p.allocator, node);
    return i;
}

fn listToSpan(p: *Parser, list: []const Ast.Node.Index) !Ast.Node.Index {
    try p.ast.extra_data.appendSlice(p.allocator, list);
    return p.addNode(.{
        .tag = .span,
        .main_token = undefined,
        .lhs = @intCast(Ast.Node.Index, p.ast.extra_data.items.len - list.len),
        .rhs = @intCast(Ast.Node.Index, p.ast.extra_data.items.len),
    });
}

fn addExtra(p: *Parser, extra: anytype) error{OutOfMemory}!Ast.Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try p.ast.extra_data.ensureUnusedCapacity(p.allocator, fields.len);
    const result = @intCast(Ast.Node.Index, p.ast.extra_data.items.len);
    inline for (fields) |field| {
        comptime std.debug.assert(field.type == Ast.Node.Index or field.type == TokenList.Index);
        p.ast.extra_data.appendAssumeCapacity(@field(extra, field.name));
    }
    return result;
}

pub fn addError(p: *Parser, loc: Token.Loc, comptime err_fmt: []const u8, fmt_args: anytype, notes: []const []const u8) void {
    var bw = std.io.bufferedWriter(p.error_file.writer());
    const b = bw.writer();
    const term = std.debug.TTY.Config{ .escape_codes = {} };
    const loc_extra = loc.extraInfo(p.source);

    if (p.failed) b.writeByte('\n') catch unreachable;

    // 'file:line:column'
    term.setColor(b, .Bold) catch unreachable;
    b.print(":{d}:{d} ", .{ loc_extra.line, loc_extra.col }) catch unreachable;

    // 'error: '
    term.setColor(b, .Red) catch unreachable;
    b.writeAll("error: ") catch unreachable;

    // error message
    term.setColor(b, .Reset) catch unreachable;
    term.setColor(b, .Bold) catch unreachable;
    b.print(err_fmt, fmt_args) catch unreachable;
    b.writeByte('\n') catch unreachable;

    // error line
    term.setColor(b, .Dim) catch unreachable;
    b.print("{d} │ ", .{loc_extra.line}) catch unreachable;
    term.setColor(b, .Reset) catch unreachable;
    b.writeAll(p.source[loc_extra.line_start..loc.start]) catch unreachable;
    term.setColor(b, .Green) catch unreachable;
    b.writeAll(p.source[loc.start..loc.end]) catch unreachable;
    term.setColor(b, .Reset) catch unreachable;
    b.writeAll(p.source[loc.end..loc_extra.line_end]) catch unreachable;
    b.writeByte('\n') catch unreachable;

    // error location pointer
    const line_number_length = (std.math.log10(loc_extra.line) + 1) + 3;
    b.writeByteNTimes(
        ' ',
        line_number_length + (loc_extra.col - 1),
    ) catch unreachable;
    term.setColor(b, .Bold) catch unreachable;
    term.setColor(b, .Green) catch unreachable;
    b.writeByte('^') catch unreachable;
    b.writeByteNTimes('~', loc.end - loc.start - 1) catch unreachable;
    b.writeByte('\n') catch unreachable;

    // notes
    for (notes) |note| {
        term.setColor(b, .Cyan) catch unreachable;
        b.writeAll("note: ") catch unreachable;

        // note message
        term.setColor(b, .Reset) catch unreachable;
        term.setColor(b, .Bold) catch unreachable;
        b.writeAll(note) catch unreachable;
        b.writeByte('\n') catch unreachable;
    }

    // clean up and flush
    term.setColor(b, .Reset) catch unreachable;
    bw.flush() catch unreachable;

    p.failed = true;
}
