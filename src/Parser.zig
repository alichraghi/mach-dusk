const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @This();

allocator: std.mem.Allocator,
source: [:0]const u8,
ast: Ast,
tok_i: u32,
error_file: std.fs.File,
failed: bool = false,

/// GlobalDecl
///   : SEMICOLON
///   | GlobalVariableDecl   SEMICOLON
///   | GlobalConstDecl      SEMICOLON
///   | GlobalOverrideDecl   SEMICOLON
///   | TypeAliasDecl        SEMICOLON
///   | StructDecl
///   | FunctionDecl         TODO
///   | ConstAssertStatement SEMICOLON
pub fn expectGlobalDecl(self: *Parser) !Ast.Node.Index {
    while (self.eatToken(.semicolon)) |_| {}

    const attrs = try self.attributeList();
    if (try self.globalVarDecl(attrs)) |node| {
        _ = try self.expectToken(.semicolon);
        return node;
        // } else if (try self.typeAliasDecl()) |type_alias| {
        //     try self.addGlobal(.{ .type_alias = type_alias });
        //     _ = try self.expectToken(.semicolon);
        // } else if (try self.globalConstDecl()) |const_decl| {
        //     try self.addGlobal(.{ .@"const" = const_decl });
        //     _ = try self.expectToken(.semicolon);
        // } else if (try self.globalOverrideDecl(attrs)) |override| {
        //     try self.addGlobal(.{ .override = override });
        //     _ = try self.expectToken(.semicolon);
        // } else if (try self.structDecl()) |strct| {
        //     try self.addGlobal(.{ .@"struct" = strct });
        // } else if (try self.constAssert()) |assert| {
        //     try self.addGlobal(.{ .const_assert = assert });
        //     _ = try self.expectToken(.semicolon);
        // } else if (try self.functionDecl(attrs)) |func| {
        //     try self.addGlobal(.{ .function = func });
    }

    self.addError(
        self.currentToken().loc,
        "expected global declaration, found '{s}'",
        .{self.currentToken().tag.symbol()},
        &.{},
    );
    return error.Parsing;
}

pub fn attributeList(self: *Parser) !?Ast.Node.Index {
    const scratch_top = self.ast.scratch.items.len;
    defer self.ast.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const attr = try self.attribute() orelse break;
        try self.ast.scratch.append(self.allocator, attr);
    }
    const list = self.ast.scratch.items[scratch_top..];
    if (list.len == 0) return null;
    return try self.listToSpan(list);
}

/// Attribute :
/// ATTR 'invariant'
/// ATTR 'const'
/// ATTR 'vertex'
/// ATTR 'fragment'
/// ATTR 'compute'
/// ATTR 'align'          PAREN_LEFT Expr                                           COMMA? PAREN_RIGHT
/// ATTR 'binding'        PAREN_LEFT Expr                                           COMMA? PAREN_RIGHT
/// ATTR 'group'          PAREN_LEFT Expr                                           COMMA? PAREN_RIGHT
/// ATTR 'id'             PAREN_LEFT Expr                                           COMMA? PAREN_RIGHT
/// ATTR 'location'       PAREN_LEFT Expr                                           COMMA? PAREN_RIGHT
/// ATTR 'size'           PAREN_LEFT Expr                                           COMMA? PAREN_RIGHT
/// ATTR 'workgroup_size' PAREN_LEFT Expr              (COMMA Expr)?  (COMMA Expr)? COMMA? PAREN_RIGHT
/// ATTR 'builtin'        PAREN_LEFT BuiltinValue                                   COMMA? PAREN_RIGHT
/// ATTR 'interpolate'    PAREN_LEFT InterpolationType (COMMA InterpolationSample)? COMMA? PAREN_RIGHT
pub fn attribute(self: *Parser) !?Ast.Node.Index {
    const attr_token = self.eatToken(.attr) orelse return null;
    const ident_tok = try self.expectToken(.ident);
    const str = self.tokenAt(ident_tok).loc.asStr(self.source);
    if (std.mem.eql(u8, "invariant", str)) {
        return try self.addNode(.{ .tag = .attr_invariant, .main_token = attr_token });
    } else if (std.mem.eql(u8, "const", str)) {
        return try self.addNode(.{ .tag = .attr_const, .main_token = attr_token });
    } else if (std.mem.eql(u8, "vertex", str)) {
        return try self.addNode(.{ .tag = .attr_vertex, .main_token = attr_token });
    } else if (std.mem.eql(u8, "fragment", str)) {
        return try self.addNode(.{ .tag = .attr_fragment, .main_token = attr_token });
    } else if (std.mem.eql(u8, "compute", str)) {
        return try self.addNode(.{ .tag = .attr_compute, .main_token = attr_token });
    } else if (std.mem.eql(u8, "align", str)) {
        _ = try self.expectToken(.paren_left);
        const expr = try self.expression() orelse {
            self.addError(self.currentToken().loc, "expected align expression", .{}, &.{});
            return error.Parsing;
        };
        _ = self.eatToken(.comma);
        _ = try self.expectToken(.paren_right);
        return try self.addNode(.{ .tag = .attr_align, .main_token = attr_token, .lhs = expr });
    } else if (std.mem.eql(u8, "binding", str)) {
        _ = try self.expectToken(.paren_left);
        const expr = try self.expression() orelse {
            self.addError(self.currentToken().loc, "expected binding expression", .{}, &.{});
            return error.Parsing;
        };
        _ = self.eatToken(.comma);
        _ = try self.expectToken(.paren_right);
        return try self.addNode(.{ .tag = .attr_binding, .main_token = attr_token, .lhs = expr });
    } else if (std.mem.eql(u8, "group", str)) {
        _ = try self.expectToken(.paren_left);
        const expr = try self.expression() orelse {
            self.addError(self.currentToken().loc, "expected group expression", .{}, &.{});
            return error.Parsing;
        };
        _ = self.eatToken(.comma);
        _ = try self.expectToken(.paren_right);
        return try self.addNode(.{ .tag = .attr_group, .main_token = attr_token, .lhs = expr });
    } else if (std.mem.eql(u8, "id", str)) {
        _ = try self.expectToken(.paren_left);
        const expr = try self.expression() orelse {
            self.addError(self.currentToken().loc, "expected id expression", .{}, &.{});
            return error.Parsing;
        };
        _ = self.eatToken(.comma);
        _ = try self.expectToken(.paren_right);
        return try self.addNode(.{ .tag = .attr_id, .main_token = attr_token, .lhs = expr });
    } else if (std.mem.eql(u8, "location", str)) {
        _ = try self.expectToken(.paren_left);
        const expr = try self.expression() orelse {
            self.addError(self.currentToken().loc, "expected location expression", .{}, &.{});
            return error.Parsing;
        };
        _ = self.eatToken(.comma);
        _ = try self.expectToken(.paren_right);
        return try self.addNode(.{ .tag = .attr_location, .main_token = attr_token, .lhs = expr });
    } else if (std.mem.eql(u8, "size", str)) {
        _ = try self.expectToken(.paren_left);
        const expr = try self.expression() orelse {
            self.addError(self.currentToken().loc, "expected size expression", .{}, &.{});
            return error.Parsing;
        };
        _ = self.eatToken(.comma);
        _ = try self.expectToken(.paren_right);
        return try self.addNode(.{ .tag = .attr_size, .main_token = attr_token, .lhs = expr });
    } else if (std.mem.eql(u8, "builtin", str)) {
        _ = try self.expectToken(.paren_left);
        const value = try self.expectBuiltinValue();
        _ = self.eatToken(.comma);
        _ = try self.expectToken(.paren_right);
        return try self.addNode(.{ .tag = .attr_builtin, .main_token = attr_token, .lhs = value });
    } else if (std.mem.eql(u8, "workgroup_size", str)) {
        _ = try self.expectToken(.paren_left);

        const expr_x = try self.expression() orelse {
            self.addError(self.currentToken().loc, "expected workgroup_size x parameter", .{}, &.{});
            return error.Parsing;
        };

        if (self.eatToken(.comma)) |_| {
            if (self.currentToken().tag != .paren_right) {
                const expr_y = try self.expression() orelse {
                    self.addError(self.currentToken().loc, "expected workgroup_size y parameter", .{}, &.{});
                    return error.Parsing;
                };

                if (self.eatToken(.comma)) |_| {
                    if (self.currentToken().tag != .paren_right) {
                        const expr_z = try self.expression() orelse {
                            self.addError(self.currentToken().loc, "expected workgroup_size z parameter", .{}, &.{});
                            return error.Parsing;
                        };

                        _ = self.eatToken(.comma);
                        _ = try self.expectToken(.paren_right);
                        const extra = try self.addExtra(Ast.Node.Workgroup{
                            .y = expr_y,
                            .z = expr_z,
                        });
                        return try self.addNode(.{
                            .tag = .attr_workgroup,
                            .main_token = attr_token,
                            .lhs = expr_x,
                            .rhs = extra,
                        });
                    }
                }

                _ = try self.expectToken(.paren_right);
                const extra = try self.addExtra(Ast.Node.Workgroup{ .y = expr_y });
                return try self.addNode(.{
                    .tag = .attr_workgroup,
                    .main_token = attr_token,
                    .lhs = expr_x,
                    .rhs = extra,
                });
            }
        }

        _ = try self.expectToken(.paren_right);
        return try self.addNode(.{
            .tag = .attr_workgroup,
            .main_token = attr_token,
            .lhs = expr_x,
        });
    } else if (std.mem.eql(u8, "interpolate", str)) {
        _ = try self.expectToken(.paren_left);
        const inter_type = try self.expectInterpolationType();

        if (self.eatToken(.comma)) |_| {
            if (self.currentToken().tag != .paren_right) {
                const inter_sample = try self.expectInterpolationSample();

                _ = self.eatToken(.comma);
                _ = try self.expectToken(.paren_right);
                return try self.addNode(.{
                    .tag = .attr_interpolate,
                    .main_token = attr_token,
                    .lhs = inter_type,
                    .rhs = inter_sample,
                });
            }
        }

        _ = try self.expectToken(.paren_right);
        return try self.addNode(.{
            .tag = .attr_interpolate,
            .main_token = attr_token,
            .lhs = inter_type,
        });
    } else {
        self.addError(
            self.tokenAt(ident_tok).loc,
            "invalid attribute name",
            .{},
            &.{
                \\possible attributes are 
                \\  'invariant', 'const', 'vertex', 'fragment', 'compute',
                \\  'align', 'binding', 'group', 'id', 'location', 'size',
                \\  'workgroup_size', 'builtin' and 'interpolate'
            },
        );
        return error.Parsing;
    }
}

/// BuiltinValue
///   : 'vertex_index'
///   | 'instance_index'
///   | 'position'
///   | 'front_facing'
///   | 'frag_depth'
///   | 'local_invocation_id'
///   | 'local_invocation_index'
///   | 'global_invocation_id'
///   | 'workgroup_id'
///   | 'num_workgroups'
///   | 'sample_index'
///   | 'sample_mask'
pub fn expectBuiltinValue(self: *Parser) !Ast.TokenIndex {
    const token = self.next();
    if (self.tokenAt(token).tag == .ident) {
        const str = self.tokenAt(token).loc.asStr(self.source);
        if (std.mem.eql(u8, "vertex_index", str) or
            std.mem.eql(u8, "instance_index", str) or
            std.mem.eql(u8, "position", str) or
            std.mem.eql(u8, "front_facing", str) or
            std.mem.eql(u8, "frag_depth", str) or
            std.mem.eql(u8, "local_invocation_id", str) or
            std.mem.eql(u8, "local_invocation_index", str) or
            std.mem.eql(u8, "global_invocation_id", str) or
            std.mem.eql(u8, "workgroup_id", str) or
            std.mem.eql(u8, "num_workgroups", str) or
            std.mem.eql(u8, "sample_index", str) or
            std.mem.eql(u8, "sample_mask", str))
        {
            return token;
        }
    }

    self.addError(
        self.tokenAt(token).loc,
        "expected builtin value name, found '{s}'",
        .{self.tokenAt(token).tag.symbol()},
        &.{"see https://gpuweb.github.io/gpuweb/wgsl/#syntax-builtin_value_name for list of values"},
    );
    return error.Parsing;
}

/// InterpolationType
///   : 'perspective'
///   | 'linear'
///   | 'flat'
pub fn expectInterpolationType(self: *Parser) !Ast.TokenIndex {
    const token = self.next();
    if (self.tokenAt(token).tag == .ident) {
        const str = self.tokenAt(token).loc.asStr(self.source);
        if (std.mem.eql(u8, "perspective", str) or
            std.mem.eql(u8, "linear", str) or
            std.mem.eql(u8, "flat", str))
        {
            return token;
        }
    }

    self.addError(
        self.tokenAt(token).loc,
        "expected interpolation type name, found '{s}'",
        .{self.tokenAt(token).tag.symbol()},
        &.{"possible values are 'perspective', 'linear' and 'flat'"},
    );
    return error.Parsing;
}

/// InterpolationSample
///   : 'center'
///   | 'centroid'
///   | 'sample'
pub fn expectInterpolationSample(self: *Parser) !Ast.Node.Index {
    const token = self.next();
    if (self.tokenAt(token).tag == .ident) {
        const str = self.tokenAt(token).loc.asStr(self.source);
        if (std.mem.eql(u8, "center", str) or
            std.mem.eql(u8, "centroid", str) or
            std.mem.eql(u8, "sample", str))
        {
            return token;
        }
    }

    self.addError(
        self.tokenAt(token).loc,
        "expected interpolation sample name, found '{s}'",
        .{self.tokenAt(token).tag.symbol()},
        &.{"possible values are 'center', 'centroid' and 'sample'"},
    );
    return error.Parsing;
}

/// GlobalVarDecl : Attribute* VariableDecl (EQUAL Expr)?
pub fn globalVarDecl(self: *Parser, attrs: ?Ast.Node.Index) !?Ast.Node.Index {
    const var_token = self.eatToken(.keyword_var) orelse return null;

    var addr_space = Ast.Node.null_node;
    var access_mode = Ast.Node.null_node;
    if (self.eatToken(.less_than)) |_| {
        addr_space = try self.expectAddressSpace();
        access_mode = if (self.eatToken(.comma)) |_|
            try self.expectAccessMode()
        else
            Ast.Node.null_node;
        _ = try self.expectToken(.greater_than);
    }

    _ = try self.expectToken(.ident);
    const var_type = if (self.eatToken(.colon)) |_|
        try self.expectTypeSpecifier()
    else
        Ast.Node.null_node;

    const initializer = if (self.eatToken(.equal)) |_|
        try self.expression() orelse {
            self.addError(
                self.currentToken().loc,
                "expected initializer expression, found '{s}'",
                .{self.currentToken().tag.symbol()},
                &.{},
            );
            return error.Parsing;
        }
    else
        Ast.Node.null_node;

    const extra = try self.addExtra(.{
        .attrs = attrs orelse Ast.Node.null_node,
        .addr_space = addr_space,
        .access_mode = access_mode,
        .type = var_type,
    });
    return try self.addNode(.{
        .tag = .global_variable,
        .main_token = var_token,
        .lhs = extra,
        .rhs = initializer,
    });
}

/// GlobalConstDecl : CONST OptionalyTypedIdent EQUAL Expr
// pub fn globalConstDecl(self: *Parser) !?Ast.Const {
//     if (self.eatToken(.keyword_const) == null) return null;
//     const ident = try self.expectOptionalyTypedIdent();
//     _ = try self.expectToken(.equal);
//     const expr = try self.expression() orelse {
//         self.addError(
//             self.currentToken().loc,
//             "expected initializer expression, found '{s}'",
//             .{self.currentToken().tag.symbol()},
//             &.{},
//         );
//         return error.Parsing;
//     };

//     return .{
//         .name = ident.name,
//         .type = ident.type,
//         .value = expr,
//     };
// }

// /// GlobalOverrideDecl : Attribute* OVERRIDE OptionalyTypedIdent (EQUAL Expr)?
// pub fn globalOverrideDecl(self: *Parser, attrs: ?Ast.Range(Ast.Attribute)) !?Ast.Override {
//     if (self.eatToken(.keyword_override) == null) return null;
//     const ident = try self.expectOptionalyTypedIdent();
//     const expr = if (self.eatToken(.equal)) |_|
//         try self.expression() orelse {
//             self.addError(
//                 self.currentToken().loc,
//                 "expected initializer expression, found '{s}'",
//                 .{self.currentToken().tag.symbol()},
//                 &.{},
//             );
//             return error.Parsing;
//         }
//     else
//         null;

//     return .{
//         .name = ident.name,
//         .type = ident.type,
//         .value = expr,
//         .attrs = attrs,
//     };
// }

// /// StructDecl : STRUCT IDENT BRACE_LEFT StructMember (COMMA StructMember)* COMMA? BRACE_RIGHT
// pub fn structDecl(self: *Parser) !?Ast.Struct {
//     if (self.eatToken(.keyword_struct) == null) return null;
//     const name = try self.expectToken(.ident);
//     _ = try self.expectToken(.brace_left);

//     var members = std.ArrayList(Ast.Struct.Member).init(self.allocator);
//     errdefer members.deinit();

//     while (true) {
//         const attrs = try self.attributeList();
//         const member = try self.structMember(attrs) orelse {
//             if (attrs != null) {
//                 self.addError(
//                     self.currentToken().loc,
//                     "expected struct member, found '{s}'",
//                     .{self.currentToken().tag.symbol()},
//                     &.{},
//                 );
//                 return error.Parsing;
//             }
//             break;
//         };
//         try members.append(member);
//         _ = self.eatToken(.comma);
//     }

//     _ = try self.expectToken(.brace_right);

//     return .{
//         .name = name.loc.asStr(self.source),
//         .members = try members.toOwnedSlice(),
//     };
// }

// /// StructMember : Attribute* TypedIdent
// pub fn structMember(self: *Parser, attrs: ?Ast.Range(Ast.Attribute)) !?Ast.Struct.Member {
//     const name = self.eatToken(.ident) orelse return null;
//     _ = try self.expectToken(.colon);
//     const member_type = try self.expectTypeSpecifier();
//     return .{
//         .name = name.loc.asStr(self.source),
//         .type = member_type,
//         .attrs = attrs,
//     };
// }

// pub fn constAssert(self: *Parser) !?Ast.Node.Index(Ast.Expression) {
//     _ = self.eatToken(.keyword_const_assert) orelse return null;
//     return try self.expression() orelse {
//         self.addError(
//             self.currentToken().loc,
//             "expected expression, found '{s}'",
//             .{self.currentToken().tag.symbol()},
//             &.{},
//         );
//         return error.Parsing;
//     };
// }

// /// FunctionDecl : Attribute* FN IDENT LEFT_PAREN ParameterList RIGHT_PAREN (ARROW FunctionResult)?
// pub fn functionDecl(self: *Parser, attrs: ?Ast.Range(Ast.Attribute)) !?Ast.Function {
//     if (self.eatToken(.keyword_fn) == null) return null;

//     const name = try self.expectToken(.ident);

//     _ = try self.expectToken(.paren_left);
//     const params = try self.expectParameterList();
//     errdefer self.allocator.free(params);
//     _ = try self.expectToken(.paren_right);

//     const result = if (self.eatToken(.arrow)) |_|
//         try self.expectFunctionResult()
//     else
//         null;

//     const body = try self.block() orelse {
//         self.addError(
//             self.currentToken().loc,
//             "expected function body, found '{s}'",
//             .{self.currentToken().tag.symbol()},
//             &.{},
//         );
//         return error.Parsing;
//     };

//     return .{
//         .name = name.loc.asStr(self.source),
//         .params = params,
//         .attrs = attrs,
//         .result = result,
//         .body = body,
//     };
// }

// /// FunctionResult : Attribute* TypeSpecifier
// pub fn expectFunctionResult(self: *Parser) !Ast.Function.Result {
//     const attrs = try self.attributeList();
//     const return_type = try self.expectTypeSpecifier();
//     return .{ .attrs = attrs, .type = return_type };
// }

// /// Block : BRACE_LEFT Statement* BRACE_RIGHT
// pub fn block(self: *Parser) error{ OutOfMemory, Parsing }!?Ast.Block {
//     _ = self.eatToken(.brace_left) orelse return null;
//     const stmnt_list = try self.statementList() orelse return null;
//     _ = try self.expectToken(.brace_right);
//     return stmnt_list;
// }

// /// Statement
// ///   : SEMICOLON
// ///   | ReturnStatement           SEMICOLON
// ///   | IfStatement                            TODO
// ///   | SwitchStatement                        TODO
// ///   | LoopStatement
// ///   | ForStatement                           TODO
// ///   | WhileStatement                         TODO
// ///   | FuncCallStatement         SEMICOLON    TODO
// ///   | VariableStatement         SEMICOLON    TODO
// ///   | BreakStatement            SEMICOLON
// ///   | BreakIfStatement          SEMICOLON
// ///   | ContinuingStatement
// ///   | ContinueStatement         SEMICOLON
// ///   | DiscardStatement          SEMICOLON
// ///   | VariableUpdatingStatement SEMICOLON    TODO
// ///   | CompoundStatement                      TODO
// ///   | ConstAssertStatement      SEMICOLON
// ///
// /// for simplicity and better error messages,
// /// we are putting all statements here
// pub fn statement(self: *Parser) !?Ast.Statement {
//     while (self.eatToken(.semicolon)) |_| {}

//     if (try self.returnStatement()) |expr| {
//         _ = try self.expectToken(.semicolon);
//         return .{ .@"return" = expr };
//     } else if (try self.loopStatement()) |blk| {
//         return .{ .loop = blk };
//     } else if (try self.continuingStatement()) |blk| {
//         return .{ .continuing = blk };
//     } else if (try self.breakIfStatement()) |expr| {
//         _ = try self.expectToken(.semicolon);
//         return .{ .break_if = expr };
//     } else if (try self.breakStatement()) |_| {
//         _ = try self.expectToken(.semicolon);
//         return .@"break";
//     } else if (try self.discardStatement()) |_| {
//         _ = try self.expectToken(.semicolon);
//         return .discard;
//     } else if (try self.continueStatement()) |_| {
//         _ = try self.expectToken(.semicolon);
//         return .@"continue";
//     } else if (try self.constAssert()) |expr| {
//         _ = try self.expectToken(.semicolon);
//         return .{ .const_assert = expr };
//     }

//     return null;
// }

// /// ReturnStatement : RETURN Expr?
// pub fn returnStatement(self: *Parser) !?Ast.Statement.Return {
//     if (self.eatToken(.keyword_return) == null) return null;
//     return if (try self.expression()) |expr| .{ .expr = expr } else .void;
// }

// /// DiscardStatement : DISCARD
// pub fn discardStatement(self: *Parser) !?void {
//     if (self.eatToken(.keyword_discard) == null) return null;
// }

// /// LoopStatement : LOOP Block
// pub fn loopStatement(self: *Parser) !?Ast.Block {
//     if (self.eatToken(.keyword_loop) == null) return null;
//     return self.block();
// }

// /// ContinuingStatement : continuing Block
// pub fn continuingStatement(self: *Parser) !?Ast.Block {
//     if (self.eatToken(.keyword_continuing) == null) return null;
//     return self.block();
// }

// /// BreakIfStatement : BREAK IF Expr
// pub fn breakIfStatement(self: *Parser) !?Ast.Node.Index(Ast.Expression) {
//     if (self.currentToken().tag == .keyword_break and self.peek().tag == .keyword_if) {
//         _ = self.next();
//         _ = self.next();
//         return self.expression();
//     }
//     return null;
// }

// /// BreakStatement : BREAK
// pub fn breakStatement(self: *Parser) !?void {
//     if (self.eatToken(.keyword_break) == null) return null;
// }

// /// ContinueStatement : CONTINUE
// pub fn continueStatement(self: *Parser) !?void {
//     if (self.eatToken(.keyword_continue) == null) return null;
// }

/// IfStatement : IF EXpr Block (ELSE IF Expr Block)* ELSE Block?
// pub fn ifStatement(self: *Parser) !?void {
//     if (self.eatToken(.keyword_if) == null) return null;

//     var stmnts = std.ArrayList(Ast.Statement).init(self.allocator);
//     defer stmnts.deinit();
//     while (true) {
//         const cond = self.expression() orelse {
//             self.addError(
//                 self.currentToken().loc,
//                 "expected condition expression, found '{s}'",
//                 .{self.currentToken().tag.symbol()},
//                 &.{},
//             );
//             return error.Parsing;
//         };
//         const payload = try self.block() orelse {
//             self.addError(
//                 self.currentToken().loc,
//                 "expected payload block, found '{s}'",
//                 .{self.currentToken().tag.symbol()},
//                 &.{},
//             );
//             return error.Parsing;
//         };
//         try stmnts.append(.{ .cond = cond, .payload = payload });

//         if (self.eatToken(.@"else")) |_| {}
//     }
// }

/// StatementList : Statement*
// pub fn statementList(self: *Parser) !?Ast.Block {
//     var stmnts = std.ArrayList(Ast.Statement).init(self.allocator);
//     defer stmnts.deinit();
//     while (try self.statement()) |stmnt| {
//         try stmnts.append(stmnt);
//     }
//     return try self.addStmntSlice(stmnts.items);
// }

// /// ParameterList : Parameter (COMMA Param)* COMMA?
// pub fn expectParameterList(self: *Parser) ![]const Ast.Function.Param {
//     var params = std.ArrayList(Ast.Function.Param).init(self.allocator);
//     errdefer params.deinit();

//     while (true) {
//         const param_token = self.currentToken();
//         const param = try self.parameter() orelse break;
//         if (params.items.len > max_call_args) {
//             self.addError(
//                 param_token.loc,
//                 "exceeded maximum function parameters ({})",
//                 .{max_call_args},
//                 &.{},
//             );
//             return error.Parsing;
//         }
//         try params.append(param);
//         if (self.eatToken(.comma) == null) break;
//     }

//     return params.toOwnedSlice();
// }

// /// Parameter : Attribute* IDENT COLON TypeSpecifier
// pub fn parameter(self: *Parser) !?Ast.Function.Param {
//     const attrs = try self.attributeList();
//     const name = if (attrs == null)
//         self.eatToken(.ident) orelse return null
//     else
//         try self.expectToken(.ident);
//     _ = try self.expectToken(.colon);
//     const param_type = try self.expectTypeSpecifier();
//     return .{
//         .name = name.loc.asStr(self.source),
//         .type = param_type,
//         .attrs = attrs,
//     };
// }

// /// TypeAliasDecl : TYPE IDENT EQUAL TypeSpecifier
// pub fn typeAliasDecl(self: *Parser) !?Ast.TypeAlias {
//     if (self.currentToken().tag != .keyword_type) return null;
//     _ = self.next();

//     const name = try self.expectToken(.ident);
//     _ = try self.expectToken(.equal);
//     const value = try self.expectTypeSpecifier();

//     return .{ .name = name.loc.asStr(self.source), .type = value };
// }

// /// VariableStatement
// ///   : VariableDecl                          TODO
// ///   | VariableDecl       EQUAL Expr         TODO
// ///   | LET   OptionalType EQUAL Expr
// ///   | CONST OptionalType EQUAL Expr
// pub fn varStatement(self: *Parser) !Ast.Variable {
//     switch (self.currentToken().tag) {
//         .keyword_let, .keyword_const => {
//             _ = self.next();

//             // const opt_type_ident = try self.expectOptionalyTypedIdent();
//             // _ = try self.expectToken(.equal);
//             // const value = self.expression() catch |err| {
//             //     if (err == error.Parsing) {
//             //         self.addError(
//             //             self.currentToken().loc,
//             //             "expected initializer expression, found '{s}'",
//             //             .{self.currentToken().tag.symbol()},
//             //             &.{},
//             //         );
//             //     }
//             //     return err;
//             // };
//             // _ = try self.expectToken(.semicolon);

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

pub fn expectTypeSpecifier(self: *Parser) error{ OutOfMemory, Parsing }!Ast.Node.Index {
    return try self.typeSpecifier() orelse {
        self.addError(
            self.currentToken().loc,
            "expected type sepecifier, found '{s}'",
            .{self.currentToken().tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
}

/// TypeSpecifier : IDENTIFIER | TypeSpecifierWithoutIdent
pub fn typeSpecifier(self: *Parser) !?Ast.Node.Index {
    if (self.currentToken().tag == .ident) {
        _ = self.next();
        return try self.addNode(.{
            .tag = .user_type,
            .main_token = self.tok_i,
        });
    }
    return self.typeSpecifierWithoutIdent();
}

/// TypeSpecifierWithoutIdent
///   : BOOL
///   | F16
///   | F32
///   | I32
///   | U32
///   | ARRAY        LESS_THAN TypeSpecifier ( COMMA ElementCountExpr )? GREATER_THAN
///   | ATOMIC       LESS_THAN TypeSpecifier GREATER_THAN
///   | PTR          LESS_THAN AddressSpace  COMMA TypeSpecifier ( COMMA AccessMode )? GREATER_THAN
///   | MatrixPrefix LESS_THAN TypeSpecifier GREATER_THAN
///   | VectorPrefix LESS_THAN TypeSpecifier GREATER_THAN
///   | texture_and_sampler_types TODO
pub fn typeSpecifierWithoutIdent(self: *Parser) !?Ast.Node.Index {
    if (self.vectorPrefix()) |vec| {
        _ = try self.expectToken(.less_than);
        const elem_type = try self.expectTypeSpecifier();
        _ = try self.expectToken(.greater_than);
        return try self.addNode(.{
            .tag = .vector_type,
            .main_token = vec,
            .lhs = elem_type,
        });
    }

    if (self.matrixPrefix()) |vec| {
        _ = try self.expectToken(.less_than);
        const elem_type = try self.expectTypeSpecifier();
        _ = try self.expectToken(.greater_than);
        return try self.addNode(.{
            .tag = .matrix_type,
            .main_token = vec,
            .lhs = elem_type,
        });
    }

    const token = self.tok_i;
    switch (self.tokenAt(token).tag) {
        .keyword_i32,
        .keyword_u32,
        .keyword_f32,
        .keyword_f16,
        .keyword_bool,
        => {
            _ = self.next();
            return try self.addNode(.{
                .tag = .scalar_type,
                .main_token = token,
            });
        },
        .keyword_sampler, .keyword_comparison_sampler => {
            _ = self.next();
            return try self.addNode(.{
                .tag = .sampler_type,
                .main_token = token,
            });
        },
        .keyword_atomic => {
            _ = self.next();
            _ = try self.expectToken(.less_than);
            const elem_type = try self.expectTypeSpecifier();
            _ = try self.expectToken(.greater_than);
            return try self.addNode(.{
                .tag = .atomic_type,
                .main_token = token,
                .lhs = elem_type,
            });
        },
        .keyword_array => {
            _ = self.next();
            _ = try self.expectToken(.less_than);
            const elem_type = try self.expectTypeSpecifier();
            if (self.eatToken(.comma)) |_| {
                const expr_token = self.currentToken();
                const size = try self.elementCountExpr() orelse {
                    self.addError(
                        expr_token.loc,
                        "expected array size expression, found '{s}'",
                        .{expr_token.tag.symbol()},
                        &.{},
                    );
                    return error.Parsing;
                };
                _ = try self.expectToken(.greater_than);
                return try self.addNode(.{
                    .tag = .array_type,
                    .main_token = token,
                    .lhs = elem_type,
                    .rhs = size,
                });
            }
            _ = try self.expectToken(.greater_than);
            return try self.addNode(.{
                .tag = .array_type,
                .main_token = token,
                .lhs = elem_type,
            });
        },
        .keyword_ptr => {
            _ = self.next();
            _ = try self.expectToken(.less_than);

            const addr_space = try self.expectAddressSpace();
            _ = try self.expectToken(.comma);
            const elem_type = try self.expectTypeSpecifier();
            const access_mode = if (self.eatToken(.comma)) |_|
                try self.expectAccessMode()
            else
                Ast.Node.null_node;
            _ = try self.expectToken(.greater_than);

            const extra = try self.addExtra(Ast.Node.PtrType{
                .addr_space = addr_space,
                .access_mode = access_mode,
            });
            return try self.addNode(.{
                .tag = .ptr_type,
                .main_token = token,
                .lhs = elem_type,
                .rhs = extra,
            });
        },
        else => return null,
    }
}

/// VectorPrefix
///   : 'vec2'
///   | 'vec3'
///   | 'vec4'
pub fn vectorPrefix(self: *Parser) ?Ast.TokenIndex {
    switch (self.currentToken().tag) {
        .keyword_vec2,
        .keyword_vec3,
        .keyword_vec4,
        => {
            _ = self.next();
            return self.tok_i;
        },
        else => return null,
    }
}

/// MatrixPrefix
///   : 'mat2x2'
///   | 'mat2x3'
///   | 'mat2x4'
///   | 'mat3x2'
///   | 'mat3x3'
///   | 'mat3x4'
///   | 'mat4x2'
///   | 'mat4x3'
///   | 'mat4x4'
pub fn matrixPrefix(self: *Parser) ?Ast.TokenIndex {
    switch (self.currentToken().tag) {
        .keyword_mat2x2,
        .keyword_mat2x3,
        .keyword_mat2x4,
        .keyword_mat3x2,
        .keyword_mat3x3,
        .keyword_mat3x4,
        .keyword_mat4x2,
        .keyword_mat4x3,
        .keyword_mat4x4,
        => {
            _ = self.next();
            return self.tok_i;
        },
        else => return null,
    }
}

/// AddressSpace
///   : 'function'
///   | 'private'
///   | 'workgroup'
///   | 'uniform'
///   | 'storage'
pub fn expectAddressSpace(self: *Parser) !Ast.TokenIndex {
    const token = self.tok_i;
    if (self.tokenAt(token).tag == .ident) {
        const str = self.tokenAt(token).loc.asStr(self.source);
        if (std.mem.eql(u8, "function", str) or
            std.mem.eql(u8, "private", str) or
            std.mem.eql(u8, "workgroup", str) or
            std.mem.eql(u8, "uniform", str) or
            std.mem.eql(u8, "storage", str))
        {
            _ = self.next();
            return token;
        }
    }

    self.addError(
        self.tokenAt(token).loc,
        "expected address space, found '{s}'",
        .{self.tokenAt(token).tag.symbol()},
        &.{"must be one of 'function', 'private', 'workgroup', 'uniform', 'storage'"},
    );
    return error.Parsing;
}

/// AccessMode : 'read' | 'write' | 'read_write'
pub fn expectAccessMode(self: *Parser) !Ast.TokenIndex {
    const token = self.tok_i;
    if (self.tokenAt(token).tag == .ident) {
        const str = self.tokenAt(token).loc.asStr(self.source);
        if (std.mem.eql(u8, "read", str) or
            std.mem.eql(u8, "write", str) or
            std.mem.eql(u8, "read_write", str))
        {
            _ = self.next();
            return token;
        }
    }

    self.addError(
        self.tokenAt(token).loc,
        "expected access mode, found '{s}'",
        .{self.tokenAt(token).tag.symbol()},
        &.{"must be one of 'read', 'write', 'read_write'"},
    );
    return error.Parsing;
}

/// PrimaryExpr
///   : Literal
///   | ParenExpr
///   | CallExpr
///   | IDENT      ArgumentExprList?
///   | BITCAST    LESS_THAN TypeSpecifier GREATER_THAN ParenExpr
pub fn primaryExpr(self: *Parser) !?Ast.Node.Index {
    if (try self.callExpr()) |call| {
        return call;
    }

    const token = self.tok_i;
    switch (self.tokenAt(token).tag) {
        .keyword_true, .keyword_false => {
            _ = self.next();
            return try self.addNode(.{ .tag = .bool_literal, .main_token = token });
        },
        .number => {
            _ = self.next();
            return try self.addNode(.{ .tag = .number_literal, .main_token = token });
        },
        .keyword_bitcast => {
            _ = self.next();
            _ = try self.expectToken(.less_than);
            const dest_type = try self.expectTypeSpecifier();
            _ = try self.expectToken(.greater_than);
            const expr = try self.expectParenExpr();
            return try self.addNode(.{
                .tag = .bitcast_expr,
                .main_token = token,
                .lhs = dest_type,
                .rhs = expr,
            });
        },
        .paren_left => return try self.expectParenExpr(),
        .ident => {
            _ = self.next();
            return try self.addNode(.{ .tag = .ident_expr, .main_token = token });
        },
        else => return null,
    }
}

/// ParenExpr : PAREN_LEFT Expr PAREN_RIGHT
pub fn expectParenExpr(self: *Parser) !Ast.Node.Index {
    _ = try self.expectToken(.paren_left);
    const expr = try self.expression() orelse {
        self.addError(
            self.currentToken().loc,
            "unable to parse expression '{s}'",
            .{self.currentToken().tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
    _ = try self.expectToken(.paren_right);
    return expr;
}

/// CallExpr
///   : ( TypeSpecifierWithoutIdent
///   |   ARRAY
///   |   MatrixPrefix
///   |   VectorPrefix )
///   ArgumentExprList
pub fn callExpr(self: *Parser) !?Ast.Node.Index {
    if (self.currentToken().tag == .ident) {
        if (self.peek().tag == .paren_left) {
            const ident_token = self.next();
            const args = try self.expectArgumentExprList();
            return try self.addNode(.{
                .tag = .call_expr,
                .main_token = ident_token,
                .rhs = args,
            });
        }
        return null;
    }

    if (self.peek().tag != .less_than) {
        if (self.vectorPrefix()) |vec| {
            const args = try self.expectArgumentExprList();
            return try self.addNode(.{ .tag = .call_expr, .main_token = vec, .rhs = args });
        }
        if (self.matrixPrefix()) |mat| {
            const args = try self.expectArgumentExprList();
            return try self.addNode(.{ .tag = .call_expr, .main_token = mat, .rhs = args });
        }
        if (self.currentToken().tag == .keyword_array) {
            const arr_token = self.next();
            const args = try self.expectArgumentExprList();
            return try self.addNode(.{ .tag = .call_expr, .main_token = arr_token, .rhs = args });
        }
    }

    const constructor = try self.typeSpecifierWithoutIdent() orelse return null;
    const args = try self.expectArgumentExprList();
    const constructor_main_token = self.ast.nodes.items(.main_token)[constructor];
    const constructor_tag = self.ast.nodes.items(.tag)[constructor];
    switch (constructor_tag) {
        .scalar_type,
        .vector_type,
        .matrix_type,
        .array_type,
        => return try self.addNode(.{
            .tag = .call_expr,
            .main_token = constructor_main_token,
            .lhs = constructor,
            .rhs = args,
        }),
        else => {
            self.addError(
                self.tokenAt(constructor_main_token).loc,
                "type '{s}' can't be called (not expression)",
                .{self.tokenAt(constructor_main_token).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        },
    }
}

/// ArgumentExprList : PAREN_LEFT ((Expr COMMA)* Expr COMMA?)? PAREN_RIGHT
pub fn expectArgumentExprList(self: *Parser) !Ast.Node.Index {
    _ = try self.expectToken(.paren_left);

    const scratch_top = self.ast.scratch.items.len;
    defer self.ast.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const expr = try self.expression() orelse break;
        try self.ast.scratch.append(self.allocator, expr);

        if (self.eatToken(.comma) == null) break;
    }

    _ = try self.expectToken(.paren_right);

    const list = self.ast.scratch.items[scratch_top..];
    return self.listToSpan(list);
}

/// ElementCountExpr
///   : UnaryExpr MathExpr
///   | UnaryExpr BitwiseExpr
pub fn elementCountExpr(self: *Parser) !?Ast.Node.Index {
    const left = try self.unaryExpr() orelse return null;
    if (try self.bitwiseExpr(left)) |right| return right;
    return try self.expectMathExpr(left);
}

/// UnaryExpr
///   : SingularExpr
///   | MINUS UnaryExpr
///   | BANG  UnaryExpr
///   | TILDE UnaryExpr
///   | STAR  UnaryExpr
///   | AND   UnaryExpr
pub fn unaryExpr(self: *Parser) error{ OutOfMemory, Parsing }!?Ast.Node.Index {
    const op_token = self.tok_i;
    const op: Ast.Node.Tag = switch (self.tokenAt(op_token).tag) {
        .bang, .tilde => .not,
        .minus => .negate,
        .star => .deref,
        .@"and" => .addr_of,
        else => return self.singularExpr(),
    };
    _ = self.next();

    const expr = try self.unaryExpr() orelse {
        self.addError(
            self.currentToken().loc,
            "unable to parse right side of '{s}' expression",
            .{self.tokenAt(op_token).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };

    return try self.addNode(.{
        .tag = op,
        .main_token = op_token,
        .lhs = expr,
    });
}

/// SingularExpr : PrimaryExpr PostfixExpr
pub fn singularExpr(self: *Parser) !?Ast.Node.Index {
    return self.primaryExpr();
    // TODO: component_or_swizzle_specifier
}

/// MultiplicativeExpr : UnaryExpr | (STAR | DIVISION | MOD MultiplicativeExpr)*
///
/// expects UnaryExpr
pub fn expectMultiplicativeExpr(self: *Parser, lhs_unary: Ast.Node.Index) !Ast.Node.Index {
    var lhs = lhs_unary;
    while (true) {
        const op_token = self.tok_i;
        const node_tag: Ast.Node.Tag = switch (self.currentToken().tag) {
            .star => .mul,
            .division => .div,
            .mod => .mod,
            else => return lhs,
        };
        _ = self.next();
        const rhs = try self.unaryExpr() orelse {
            self.addError(
                self.currentToken().loc,
                "unable to parse right side of '{s}' expression",
                .{self.currentToken().tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
        lhs = try self.addNode(.{
            .tag = node_tag,
            .main_token = op_token,
            .lhs = lhs,
            .rhs = rhs,
        });
    }
}

/// ComponentOrSwizzleSpecifier
///   :
///   | BRACE_LEFT Expr BRACE_RIGHT ComponentOrSwizzleSpecifier?
///   | PERIOD MemberIdent ComponentOrSwizzleSpecifier?
///   | PERIOD SwizzleName ComponentOrSwizzleSpecifier?
//
//  =============================================================
//
/// AdditiveExpr : MultiplicativeExpr | (PLUS | MINUS AdditiveExpr)*
///
/// expects first expression ( MultiplicativeExpr )
pub fn expectAdditiveExpr(self: *Parser, lhs_mul: Ast.Node.Index) !Ast.Node.Index {
    var lhs = lhs_mul;
    while (true) {
        const op_token = self.tok_i;
        const op: Ast.Node.Tag = switch (self.tokenAt(op_token).tag) {
            .plus => .add,
            .minus => .sub,
            else => return lhs,
        };
        _ = self.next();
        const unary = try self.unaryExpr() orelse {
            self.addError(
                self.currentToken().loc,
                "unable to parse right side of '{s}' expression",
                .{self.tokenAt(op_token).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
        const rhs = try self.expectMultiplicativeExpr(unary);
        lhs = try self.addNode(.{
            .tag = op,
            .main_token = op_token,
            .lhs = lhs,
            .rhs = rhs,
        });
    }
}

/// MathExpr : MultiplicativeExpr AdditiveExpr
pub fn expectMathExpr(self: *Parser, left: Ast.Node.Index) !Ast.Node.Index {
    const right = try self.expectMultiplicativeExpr(left);
    return self.expectAdditiveExpr(right);
}

/// ShiftExpr
///   : MathExpr
///   | UnaryExpr SHIFT_LEFT  UnaryExpr
///   | UnaryExpr SHIFT_RIGHT UnaryExpr
///
/// expects first expression ( UnaryExpr )
pub fn expectShiftExpr(self: *Parser, lhs: Ast.Node.Index) !Ast.Node.Index {
    const op_token = self.tok_i;
    const op: Ast.Node.Tag = switch (self.tokenAt(op_token).tag) {
        .shift_left => .shift_left,
        .shift_right => .shift_right,
        else => return try self.expectMathExpr(lhs),
    };
    _ = self.next();

    const rhs = try self.unaryExpr() orelse {
        self.addError(
            self.currentToken().loc,
            "unable to parse right side of '{s}' expression",
            .{self.tokenAt(op_token).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };

    return try self.addNode(.{
        .tag = op,
        .main_token = op_token,
        .lhs = lhs,
        .rhs = rhs,
    });
}

// RelationalExpr
//   : ShiftExpr
//   | ShiftExpr EQUAL_EQUAL        ShiftExpr
//   | ShiftExpr GREATER_THAN       ShiftExpr
//   | ShiftExpr GREATER_THAN_EQUAL ShiftExpr
//   | ShiftExpr LESS_THAN          ShiftExpr
//   | ShiftExpr LESS_THAN_EQUAL    ShiftExpr
//   | ShiftExpr NOT_EQUAL          ShiftExpr
///
/// expects first expression ( UnaryExpr )
pub fn expectRelationalExpr(self: *Parser, lhs_unary: Ast.Node.Index) !Ast.Node.Index {
    const lhs = try self.expectShiftExpr(lhs_unary);
    const op_token = self.tok_i;
    const op: Ast.Node.Tag = switch (self.tokenAt(op_token).tag) {
        .equal_equal => .equal,
        .not_equal => .not_equal,
        .less_than => .less,
        .less_than_equal => .less_equal,
        .greater_than => .greater,
        .greater_than_equal => .greater_equal,
        else => return lhs,
    };
    _ = self.next();

    const rhs_unary = try self.unaryExpr() orelse {
        self.addError(
            self.currentToken().loc,
            "unable to parse right side of '{s}' expression",
            .{self.tokenAt(op_token).tag.symbol()},
            &.{},
        );
        return error.Parsing;
    };
    const rhs = try self.expectShiftExpr(rhs_unary);
    return try self.addNode(.{
        .tag = op,
        .main_token = op_token,
        .lhs = lhs,
        .rhs = rhs,
    });
}

/// BitwiseExpr
///   : UnaryExpr AND UnaryExpr (AND UnaryExpr)*
///   | UnaryExpr OR  UnaryExpr (OR  UnaryExpr)*
///   | UnaryExpr XOR UnaryExpr (XOR UnaryExpr)*
///
/// expects first expression ( UnaryExpr )
pub fn bitwiseExpr(self: *Parser, lhs: Ast.Node.Index) !?Ast.Node.Index {
    const op_token = self.tok_i;
    const op: Ast.Node.Tag = switch (self.tokenAt(op_token).tag) {
        .@"and" => .binary_and,
        .@"or" => .binary_or,
        .xor => .binary_xor,
        else => return null,
    };
    _ = self.next();

    var lhs_result = lhs;
    while (true) {
        const rhs = try self.unaryExpr() orelse {
            self.addError(
                self.currentToken().loc,
                "unable to parse right side of '{s}' expression",
                .{self.tokenAt(op_token).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };

        lhs_result = try self.addNode(.{
            .tag = op,
            .main_token = op_token,
            .lhs = lhs_result,
            .rhs = rhs,
        });

        if (self.currentToken().tag != self.tokenAt(op_token).tag) return lhs_result;
    }
}

/// ShortCircuitExpr
///   : RelationalExpr
///   | RelationalExpr (AND_AND RelationalExpr)*
///   | RelationalExpr (OR_OR   RelationalExpr)*
///
/// expects first expression ( UnaryExpr )
pub fn expectShortCircuitExpr(self: *Parser, lhs_relational: Ast.Node.Index) !Ast.Node.Index {
    var lhs = lhs_relational;

    const op_token = self.tok_i;
    const op: Ast.Node.Tag = switch (self.tokenAt(op_token).tag) {
        .and_and => .circuit_and,
        .or_or => .circuit_or,
        else => return lhs,
    };

    while (self.currentToken().tag == self.tokenAt(op_token).tag) {
        _ = self.next();

        const rhs_unary = try self.unaryExpr() orelse {
            self.addError(
                self.currentToken().loc,
                "unable to parse right side of '{s}' expression",
                .{self.tokenAt(op_token).tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
        const rhs = try self.expectRelationalExpr(rhs_unary);

        lhs = try self.addNode(.{
            .tag = op,
            .main_token = op_token,
            .lhs = lhs,
            .rhs = rhs,
        });
    }

    return lhs;
}

/// Expr
///   : RelationalExpr
///   | BitwiseExpr
///   | RelationalExpr AND_AND RelationalExpr
///   | RelationalExpr OR_OR   RelationalExpr
pub fn expression(self: *Parser) !?Ast.Node.Index {
    const lhs_unary = try self.unaryExpr() orelse return null;
    if (try self.bitwiseExpr(lhs_unary)) |bitwise| return bitwise;
    const lhs = try self.expectRelationalExpr(lhs_unary);
    return try self.expectShortCircuitExpr(lhs);
}

pub fn expectToken(self: *Parser, tag: Token.Tag) !Ast.TokenIndex {
    const token = self.next();
    if (self.tokenAt(token).tag == tag) return token;

    self.addError(
        self.tokenAt(token).loc,
        "expected '{s}', but found '{s}'",
        .{ tag.symbol(), self.tokenAt(token).tag.symbol() },
        &.{},
    );
    return error.Parsing;
}

pub fn eatToken(self: *Parser, tag: Token.Tag) ?Ast.TokenIndex {
    return if (self.currentToken().tag == tag) self.next() else null;
}

pub fn peek(self: *Parser) Token {
    return self.tokenAt(std.math.min(self.tok_i + 1, self.ast.tokens.len));
}

pub fn tokenAt(self: *Parser, i: Ast.TokenIndex) Token {
    return self.ast.tokens[std.math.min(i, self.ast.tokens.len)];
}

pub fn currentToken(self: *Parser) Token {
    return self.tokenAt(self.tok_i);
}

pub fn next(self: *Parser) Ast.TokenIndex {
    const current = self.tok_i;
    self.tok_i = std.math.min(self.tok_i + 1, self.ast.tokens.len);
    return current;
}

pub fn continueUntilOrEOF(self: *Parser, until: Token.Tag) void {
    while (true) {
        const tag = self.tokenAt(self.next()).tag;
        if (tag == until or tag == .eof) break;
    }
}

pub fn addNode(self: *Parser, node: Ast.Node) error{OutOfMemory}!Ast.Node.Index {
    const i = @intCast(Ast.Node.Index, self.ast.nodes.len);
    try self.ast.nodes.append(self.allocator, node);
    return i;
}

fn listToSpan(self: *Parser, list: []const Ast.Node.Index) !Ast.Node.Index {
    try self.ast.extra_data.appendSlice(self.allocator, list);
    return self.addNode(.{
        .tag = .span,
        .main_token = undefined,
        .lhs = @intCast(Ast.Node.Index, self.ast.extra_data.items.len - list.len),
        .rhs = @intCast(Ast.Node.Index, self.ast.extra_data.items.len),
    });
}

fn addExtra(self: *Parser, extra: anytype) error{OutOfMemory}!Ast.Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try self.ast.extra_data.ensureUnusedCapacity(self.allocator, fields.len);
    const result = @intCast(Ast.Node.Index, self.ast.extra_data.items.len);
    inline for (fields) |field| {
        comptime std.debug.assert(field.type == Ast.Node.Index or field.type == Ast.TokenIndex);
        self.ast.extra_data.appendAssumeCapacity(@field(extra, field.name));
    }
    return result;
}

pub fn addError(self: *Parser, loc: Token.Loc, comptime err_fmt: []const u8, fmt_args: anytype, notes: []const []const u8) void {
    var bw = std.io.bufferedWriter(self.error_file.writer());
    const b = bw.writer();
    const term = std.debug.TTY.Config{ .escape_codes = {} };
    const loc_extra = loc.extraInfo(self.source);

    if (self.failed) b.writeByte('\n') catch unreachable;

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
    b.writeAll(self.source[loc_extra.line_start..loc_extra.line_end]) catch unreachable;
    b.writeByte('\n') catch unreachable;

    // error location pointer ('^')
    b.writeByteNTimes(
        ' ',
        (std.math.log10(loc_extra.line) + 1) + (3) + (loc_extra.col - 1),
    ) catch unreachable;
    term.setColor(b, .Bold) catch unreachable;
    term.setColor(b, .Green) catch unreachable;
    b.writeAll("^\n") catch unreachable;

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

    self.failed = true;
}
