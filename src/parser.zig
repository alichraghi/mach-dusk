const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");
const Tokenizer = @import("Tokenizer.zig");

const max_call_args = 64;

pub fn parse(
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    error_file: ?std.fs.File,
) !Ast {
    var tokenizer = Tokenizer.init(source);
    var first_token = tokenizer.next();

    var parser = Parser{
        .allocator = allocator,
        .source = source,
        .tokenizer = tokenizer,
        .current_token = first_token,
        .error_file = error_file orelse std.io.getStdErr(),
    };
    try parser.parseRoot();
    return parser.ast;
}

const Parser = struct {
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    tokenizer: Tokenizer,
    current_token: Token,
    error_file: std.fs.File,
    failed: bool = false,
    ast: Ast = .{},

    pub fn parseRoot(self: *Parser) !void {
        errdefer self.ast.deinit(self.allocator);
        try self.ast.globals.ensureTotalCapacityPrecise(self.allocator, self.source.len / 300); // 1:300 source to globals
        try self.ast.expressions.ensureTotalCapacityPrecise(self.allocator, self.source.len / 10); // 1:10 source to expressions

        while (self.current_token.tag != .eof) {
            self.globalDecl() catch |err| {
                if (err == error.Parsing) {
                    self.continueUntilOrEOF(.semicolon);
                    continue;
                } else return err;
            };
        }
    }

    /// GlobalDecl
    ///   : SEMICOLON
    ///   | GlobalVariableDecl   SEMICOLON
    ///   | GlobalConstDecl      SEMICOLON
    ///   | GlobalOverrideDecl   SEMICOLON
    ///   | TypeAliasDecl        SEMICOLON
    ///   | StructDecl
    ///   | FunctionDecl         TODO
    ///   | ConstAssertStatement SEMICOLON
    pub fn globalDecl(self: *Parser) !void {
        const attrs = try self.attributeList();
        errdefer self.allocator.free(attrs);

        if (try self.typeAliasDecl()) |type_alias| {
            try self.addGlobal(.{ .type_alias = type_alias });
            _ = try self.expectToken(.semicolon);
        } else if (try self.globalVarDecl(attrs)) |variable| {
            try self.addGlobal(.{ .variable = variable });
            _ = try self.expectToken(.semicolon);
        } else if (try self.globalConstDecl()) |const_decl| {
            try self.addGlobal(.{ .@"const" = const_decl });
            _ = try self.expectToken(.semicolon);
        } else if (try self.globalOverrideDecl(attrs)) |override| {
            try self.addGlobal(.{ .override = override });
            _ = try self.expectToken(.semicolon);
        } else if (try self.structDecl()) |strct| {
            try self.addGlobal(.{ .@"struct" = strct });
        } else if (try self.constAssert()) |assert| {
            try self.addGlobal(.{ .const_assert = assert });
        } else {
            if (attrs.len > 0) {
                self.addError(
                    self.current_token.loc,
                    "expected global declaration, found '{s}'",
                    .{self.current_token.tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            }
            _ = try self.expectToken(.semicolon);
            return;
        }
    }

    pub fn attributeList(self: *Parser) ![]const Ast.Attribute {
        const max_attrs = std.meta.fields(Ast.Attribute).len;

        var attrs = try std.ArrayList(Ast.Attribute).initCapacity(self.allocator, max_attrs);
        errdefer attrs.deinit();

        while (true) {
            if (attrs.items.len >= max_attrs) {
                attrs.deinit();
                self.addError(
                    self.current_token.loc,
                    "exceeded maximum attributes per declaration ({})",
                    .{max_attrs},
                    &.{},
                );
                return error.Parsing;
            }

            const attr = try self.attribute() orelse break;
            attrs.appendAssumeCapacity(attr);
        }

        return attrs.toOwnedSlice();
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
    pub fn attribute(self: *Parser) !?Ast.Attribute {
        if (self.eatToken(.attr) == null) return null;
        const ident_tok = try self.expectToken(.ident);
        const str = ident_tok.loc.asStr(self.source);
        const attr = std.meta.stringToEnum(std.meta.Tag(Ast.Attribute), str) orelse {
            self.addError(
                ident_tok.loc,
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
        };
        switch (attr) {
            .invariant => return .invariant,
            .@"const" => return .@"const",
            .vertex => return .vertex,
            .fragment => return .fragment,
            .compute => return .compute,
            .@"align" => {
                _ = try self.expectToken(.paren_left);
                const expr = try self.expression() orelse {
                    self.addError(self.current_token.loc, "expected align expression", .{}, &.{});
                    return error.Parsing;
                };
                _ = self.eatToken(.comma);
                _ = try self.expectToken(.paren_right);
                return .{ .@"align" = expr };
            },
            .binding => {
                _ = try self.expectToken(.paren_left);
                const expr = try self.expression() orelse {
                    self.addError(self.current_token.loc, "expected binding expression", .{}, &.{});
                    return error.Parsing;
                };
                _ = self.eatToken(.comma);
                _ = try self.expectToken(.paren_right);
                return .{ .binding = expr };
            },
            .group => {
                _ = try self.expectToken(.paren_left);
                const expr = try self.expression() orelse {
                    self.addError(self.current_token.loc, "expected group expression", .{}, &.{});
                    return error.Parsing;
                };
                _ = self.eatToken(.comma);
                _ = try self.expectToken(.paren_right);
                return .{ .group = expr };
            },
            .id => {
                _ = try self.expectToken(.paren_left);
                const expr = try self.expression() orelse {
                    self.addError(self.current_token.loc, "expected id expression", .{}, &.{});
                    return error.Parsing;
                };
                _ = self.eatToken(.comma);
                _ = try self.expectToken(.paren_right);
                return .{ .id = expr };
            },
            .location => {
                _ = try self.expectToken(.paren_left);
                const expr = try self.expression() orelse {
                    self.addError(self.current_token.loc, "expected location expression", .{}, &.{});
                    return error.Parsing;
                };
                _ = self.eatToken(.comma);
                _ = try self.expectToken(.paren_right);
                return .{ .location = expr };
            },
            .size => {
                _ = try self.expectToken(.paren_left);
                const expr = try self.expression() orelse {
                    self.addError(self.current_token.loc, "expected size expression", .{}, &.{});
                    return error.Parsing;
                };
                _ = self.eatToken(.comma);
                _ = try self.expectToken(.paren_right);
                return .{ .size = expr };
            },
            .workgroup_size => {
                _ = try self.expectToken(.paren_left);

                const expr_1 = try self.expression() orelse {
                    self.addError(self.current_token.loc, "expected workgroup_size x parameter", .{}, &.{});
                    return error.Parsing;
                };

                if (self.eatToken(.comma)) |_| {
                    if (self.current_token.tag != .paren_right) {
                        const expr_2 = try self.expression() orelse {
                            self.addError(self.current_token.loc, "expected workgroup_size y parameter", .{}, &.{});
                            return error.Parsing;
                        };

                        if (self.eatToken(.comma)) |_| {
                            if (self.current_token.tag != .paren_right) {
                                const expr_3 = try self.expression() orelse {
                                    self.addError(self.current_token.loc, "expected workgroup_size z parameter", .{}, &.{});
                                    return error.Parsing;
                                };

                                _ = self.eatToken(.comma);
                                _ = try self.expectToken(.paren_right);
                                return .{ .workgroup_size = .{ expr_1, expr_2, expr_3 } };
                            }
                        }

                        _ = try self.expectToken(.paren_right);
                        return .{ .workgroup_size = .{ expr_1, expr_2, null } };
                    }
                }

                _ = try self.expectToken(.paren_right);
                return .{ .workgroup_size = .{ expr_1, null, null } };
            },
            .builtin => {
                _ = try self.expectToken(.paren_left);
                const value = try self.expectBuiltinValue();
                _ = self.eatToken(.comma);
                _ = try self.expectToken(.paren_right);
                return .{ .builtin = value };
            },
            .interpolate => {
                _ = try self.expectToken(.paren_left);
                const inter_type = try self.expectInterpolationType();

                if (self.eatToken(.comma)) |_| {
                    if (self.current_token.tag != .paren_right) {
                        const inter_sample = try self.expectInterpolationSample();

                        _ = self.eatToken(.comma);
                        _ = try self.expectToken(.paren_right);
                        return .{ .interpolate = .{ .type = inter_type, .sample = inter_sample } };
                    }
                }

                _ = try self.expectToken(.paren_right);
                return .{ .interpolate = .{ .type = inter_type, .sample = null } };
            },
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
    pub fn expectBuiltinValue(self: *Parser) !Ast.Attribute.Builtin {
        if (self.current_token.tag == .ident) {
            const str = self.current_token.loc.asStr(self.source);
            if (std.meta.stringToEnum(Ast.Attribute.Builtin, str)) |res| {
                _ = self.next();
                return res;
            }
        }

        self.addError(
            self.current_token.loc,
            "expected builtin value name, found '{s}'",
            .{self.current_token.tag.symbol()},
            &.{"see https://gpuweb.github.io/gpuweb/wgsl/#syntax-builtin_value_name for list of values"},
        );
        return error.Parsing;
    }

    /// InterpolationType
    ///   : 'perspective'
    ///   | 'linear'
    ///   | 'flat'
    pub fn expectInterpolationType(self: *Parser) !Ast.Attribute.InterpolationType {
        if (self.current_token.tag == .ident) {
            const str = self.current_token.loc.asStr(self.source);
            if (std.meta.stringToEnum(Ast.Attribute.InterpolationType, str)) |res| {
                _ = self.next();
                return res;
            }
        }

        self.addError(
            self.current_token.loc,
            "expected interpolation type name, found '{s}'",
            .{self.current_token.tag.symbol()},
            &.{"possible values are 'perspective', 'linear' and 'flat'"},
        );
        return error.Parsing;
    }

    /// InterpolationSample
    ///   : 'center'
    ///   | 'centroid'
    ///   | 'sample'
    pub fn expectInterpolationSample(self: *Parser) !Ast.Attribute.InterpolationSample {
        if (self.current_token.tag == .ident) {
            const str = self.current_token.loc.asStr(self.source);
            if (std.meta.stringToEnum(Ast.Attribute.InterpolationSample, str)) |res| {
                _ = self.next();
                return res;
            }
        }

        self.addError(
            self.current_token.loc,
            "expected interpolation sample name, found '{s}'",
            .{self.current_token.tag.symbol()},
            &.{"possible values are 'center', 'centroid' and 'sample'"},
        );
        return error.Parsing;
    }

    /// GlobalVarDecl : Attribute* VariableDecl (EQUAL Expr)?
    pub fn globalVarDecl(self: *Parser, attrs: []const Ast.Attribute) !?Ast.Variable {
        const decl = try self.variableDecl() orelse return null;
        const initializer = if (self.eatToken(.equal)) |_|
            try self.expression() orelse {
                self.addError(
                    self.current_token.loc,
                    "expected initializer expression, found '{s}'",
                    .{self.current_token.tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            }
        else
            null;

        return .{
            .name = decl.ident.name,
            .value = initializer,
            .addr_space = if (decl.qualifier) |q| q.addr_space else null,
            .access = if (decl.qualifier) |q| q.access else null,
            .type = decl.ident.type,
            .attrs = attrs,
        };
    }

    /// GlobalConstDecl : CONST OptionalyTypedIdent EQUAL Expr
    pub fn globalConstDecl(self: *Parser) !?Ast.Const {
        if (self.eatToken(.keyword_const) == null) return null;
        const ident = try self.expectOptionalyTypedIdent();
        _ = try self.expectToken(.equal);
        const expr = try self.expression() orelse {
            self.addError(
                self.current_token.loc,
                "expected initializer expression, found '{s}'",
                .{self.current_token.tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };

        return .{
            .name = ident.name,
            .type = ident.type,
            .value = expr,
        };
    }

    /// GlobalOverrideDecl : Attribute* OVERRIDE OptionalyTypedIdent (EQUAL Expr)?
    pub fn globalOverrideDecl(self: *Parser, attrs: []const Ast.Attribute) !?Ast.Override {
        if (self.eatToken(.keyword_override) == null) return null;
        const ident = try self.expectOptionalyTypedIdent();
        const expr = if (self.eatToken(.equal)) |_|
            try self.expression() orelse {
                self.addError(
                    self.current_token.loc,
                    "expected initializer expression, found '{s}'",
                    .{self.current_token.tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            }
        else
            null;

        return .{
            .name = ident.name,
            .type = ident.type,
            .value = expr,
            .attrs = attrs,
        };
    }

    /// StructDecl : STRUCT IDENT BRACE_LEFT StructMember (COMMA StructMember)* COMMA? BRACE_RIGHT
    pub fn structDecl(self: *Parser) !?Ast.Struct {
        if (self.eatToken(.keyword_struct) == null) return null;
        const name = try self.expectToken(.ident);
        _ = try self.expectToken(.brace_left);

        var members = std.ArrayList(Ast.Struct.Member).init(self.allocator);
        errdefer members.deinit();

        while (true) {
            const attrs = try self.attributeList();
            errdefer self.allocator.free(attrs);

            const member = try self.structMember(attrs) orelse {
                if (attrs.len > 0) {
                    self.addError(
                        self.current_token.loc,
                        "expected struct member, found '{s}'",
                        .{self.current_token.tag.symbol()},
                        &.{},
                    );
                    return error.Parsing;
                }
                break;
            };
            try members.append(member);
            _ = self.eatToken(.comma);
        }

        _ = try self.expectToken(.brace_right);

        return .{
            .name = name.loc.asStr(self.source),
            .members = try members.toOwnedSlice(),
        };
    }

    /// StructMember : Attribute* TypedIdent
    pub fn structMember(self: *Parser, attrs: []const Ast.Attribute) !?Ast.Struct.Member {
        const name = self.eatToken(.ident) orelse return null;
        _ = try self.expectToken(.colon);
        const member_type = try self.expectTypeSpecifier();
        return .{
            .name = name.loc.asStr(self.source),
            .type = member_type,
            .attrs = attrs,
        };
    }

    pub fn constAssert(self: *Parser) !?Ast.Index(Ast.Expression) {
        _ = self.eatToken(.keyword_const_assert) orelse return null;
        return try self.expression() orelse {
            self.addError(
                self.current_token.loc,
                "expected expression, found '{s}'",
                .{self.current_token.tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
    }

    /// VariableDecl : VAR VariableQualifier? OptionalyTypedIdent
    pub fn variableDecl(self: *Parser) !?Ast.Variable.DeclInfo {
        if (self.eatToken(.keyword_var) == null) return null;
        const qualifier = try self.variableQualifier();
        const opt_type_id = try self.expectOptionalyTypedIdent();
        return .{
            .ident = opt_type_id,
            .qualifier = qualifier,
        };
    }

    /// VariableQualifier : LESS_THAN AddressSpace (COMMA AccessMode)? GREATER_THAN
    pub fn variableQualifier(self: *Parser) !?Ast.Variable.Qualifier {
        if (self.eatToken(.less_than) == null) return null;
        const addr_space = try self.expectAddressSpace();
        const access = if (self.eatToken(.comma)) |_|
            try self.expectAccessMode()
        else
            null;
        _ = try self.expectToken(.greater_than);
        return .{ .addr_space = addr_space, .access = access };
    }

    // OptionalyTypedIdent : IDENT ( COLON TypeSpecifier ) ?
    pub fn expectOptionalyTypedIdent(self: *Parser) !Ast.OptionalyTypedIdent {
        const name = try self.expectToken(.ident);
        const ident_type = if (self.eatToken(.colon)) |_|
            try self.expectTypeSpecifier()
        else
            null;
        return .{ .name = name.loc.asStr(self.source), .type = ident_type };
    }

    /// TypeAliasDecl : TYPE IDENT EQUAL TypeSpecifier
    pub fn typeAliasDecl(self: *Parser) !?Ast.TypeAlias {
        if (self.current_token.tag != .keyword_type) return null;
        _ = self.next();

        const name = try self.expectToken(.ident);
        _ = try self.expectToken(.equal);
        const value = try self.expectTypeSpecifier();

        return .{ .name = name.loc.asStr(self.source), .type = value };
    }

    /// VariableStatement
    ///   : VariableDecl                          TODO
    ///   | VariableDecl       EQUAL Expr         TODO
    ///   | LET   OptionalType EQUAL Expr
    ///   | CONST OptionalType EQUAL Expr
    pub fn varStatement(self: *Parser) !Ast.Variable {
        switch (self.current_token.tag) {
            .keyword_let, .keyword_const => {
                _ = self.next();

                // const opt_type_ident = try self.expectOptionalyTypedIdent();
                // _ = try self.expectToken(.equal);
                // const value = self.expression() catch |err| {
                //     if (err == error.Parsing) {
                //         self.addError(
                //             self.current_token.loc,
                //             "expected initializer expression, found '{s}'",
                //             .{self.current_token.tag.symbol()},
                //             &.{},
                //         );
                //     }
                //     return err;
                // };
                // _ = try self.expectToken(.semicolon);

                // const scope: Ast.Variable.Scope = switch (tag) {
                //     .keyword_let => .function,
                //     .keyword_const => .module_or_function,
                //     else => unreachable,
                // };

                // return .{
                //     .name = opt_type_ident.name,
                //     .type = opt_type_ident.type,
                //     .value = value,
                // };
                return error.Parsing;
            },
            else => return error.Parsing,
        }
    }

    pub fn expectTypeSpecifier(self: *Parser) error{ OutOfMemory, Parsing }!Ast.Index(Ast.Type) {
        return try self.typeSpecifier() orelse {
            self.addError(
                self.current_token.loc,
                "expected type sepecifier, found '{s}'",
                .{self.current_token.tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
    }

    /// TypeSpecifier : IDENTIFIER | TypeSpecifierWithoutIdent
    pub fn typeSpecifier(self: *Parser) !?Ast.Index(Ast.Type) {
        if (self.current_token.tag == .ident) {
            _ = self.next();
            return try self.addType(.{ .user = self.current_token.loc.asStr(self.source) });
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
    pub fn typeSpecifierWithoutIdent(self: *Parser) !?Ast.Index(Ast.Type) {
        if (self.vectorPrefix()) |vec| {
            _ = try self.expectToken(.less_than);
            const elem_type = try self.expectTypeSpecifier();
            _ = try self.expectToken(.greater_than);
            return try self.addType(.{ .vector = .{ .prefix = vec, .element = elem_type } });
        }

        if (self.matrixPrefix()) |mat| {
            _ = try self.expectToken(.less_than);
            const elem_type = try self.expectTypeSpecifier();
            _ = try self.expectToken(.greater_than);
            return try self.addType(.{ .matrix = .{ .prefix = mat, .element = elem_type } });
        }

        switch (self.current_token.tag) {
            .keyword_i32 => {
                _ = self.next();
                return try self.addType(.{ .scalar = .i32 });
            },
            .keyword_u32 => {
                _ = self.next();
                return try self.addType(.{ .scalar = .u32 });
            },
            .keyword_f32 => {
                _ = self.next();
                return try self.addType(.{ .scalar = .f32 });
            },
            .keyword_f16 => {
                _ = self.next();
                return try self.addType(.{ .scalar = .f16 });
            },
            .keyword_bool => {
                _ = self.next();
                return try self.addType(.{ .scalar = .bool });
            },
            .keyword_sampler => {
                _ = self.next();
                return try self.addType(.{ .sampler = .{ .comparison = false } });
            },
            .keyword_comparison_sampler => {
                _ = self.next();
                return try self.addType(.{ .sampler = .{ .comparison = true } });
            },
            .keyword_atomic => {
                _ = self.next();
                _ = try self.expectToken(.less_than);
                const elem_type = try self.expectTypeSpecifier();
                _ = try self.expectToken(.greater_than);
                return try self.addType(.{ .atomic = .{ .element = elem_type } });
            },
            .keyword_array => {
                _ = self.next();
                _ = try self.expectToken(.less_than);
                const elem_type = try self.expectTypeSpecifier();
                if (self.eatToken(.comma)) |_| {
                    const expr_token = self.current_token;
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
                    return try self.addType(.{ .array = .{
                        .element = elem_type,
                        .size = .{ .static = size },
                    } });
                }
                _ = try self.expectToken(.greater_than);
                return try self.addType(.{ .array = .{ .element = elem_type, .size = .dynamic } });
            },
            .keyword_ptr => {
                _ = self.next();
                _ = try self.expectToken(.less_than);
                const addr_space = try self.expectAddressSpace();
                _ = try self.expectToken(.comma);
                const ty = try self.expectTypeSpecifier();
                if (self.eatToken(.comma)) |_| {
                    const access_mode = try self.expectAccessMode();
                    _ = try self.expectToken(.greater_than);
                    return try self.addType(.{ .ptr = .{
                        .addr_space = addr_space,
                        .type = ty,
                        .access = access_mode,
                    } });
                }
                _ = try self.expectToken(.greater_than);
                return try self.addType(.{ .ptr = .{
                    .addr_space = addr_space,
                    .type = ty,
                    .access = null,
                } });
            },
            else => return null,
        }
    }

    /// VectorPrefix
    ///   : 'vec2'
    ///   | 'vec3'
    ///   | 'vec4'
    pub fn vectorPrefix(self: *Parser) ?Ast.Type.Vector.Prefix {
        const res: Ast.Type.Vector.Prefix = switch (self.current_token.tag) {
            .keyword_vec2 => .vec2,
            .keyword_vec3 => .vec3,
            .keyword_vec4 => .vec4,
            else => return null,
        };
        _ = self.next();
        return res;
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
    pub fn matrixPrefix(self: *Parser) ?Ast.Type.Matrix.Prefix {
        const res: Ast.Type.Matrix.Prefix = switch (self.current_token.tag) {
            .keyword_mat2x2 => .mat2x2,
            .keyword_mat2x3 => .mat2x3,
            .keyword_mat2x4 => .mat2x4,
            .keyword_mat3x2 => .mat3x2,
            .keyword_mat3x3 => .mat3x3,
            .keyword_mat3x4 => .mat3x4,
            .keyword_mat4x2 => .mat4x2,
            .keyword_mat4x3 => .mat4x3,
            .keyword_mat4x4 => .mat4x4,
            else => return null,
        };
        _ = self.next();
        return res;
    }

    /// AddressSpace
    ///   : 'function'
    ///   | 'private'
    ///   | 'workgroup'
    ///   | 'uniform'
    ///   | 'storage'
    pub fn expectAddressSpace(self: *Parser) !Ast.AddressSpace {
        if (self.current_token.tag == .ident) {
            const str = self.current_token.loc.asStr(self.source);
            if (std.meta.stringToEnum(Ast.AddressSpace, str)) |res| {
                _ = self.next();
                return res;
            }
        }

        self.addError(
            self.current_token.loc,
            "expected address space, found '{s}'",
            .{self.current_token.tag.symbol()},
            &.{"must be one of 'function', 'private', 'workgroup', 'uniform', 'storage'"},
        );
        return error.Parsing;
    }

    /// AccessMode : 'read' | 'write' | 'read_write'
    pub fn expectAccessMode(self: *Parser) !Ast.AccessMode {
        if (self.current_token.tag == .ident) {
            const str = self.current_token.loc.asStr(self.source);
            if (std.meta.stringToEnum(Ast.AccessMode, str)) |res| {
                _ = self.next();
                return res;
            }
        }

        self.addError(
            self.current_token.loc,
            "expected access mode, found '{s}'",
            .{self.current_token.tag.symbol()},
            &.{"must be one of 'read', 'write', 'read_write'"},
        );
        return error.Parsing;
    }

    /// PrimaryExpr
    ///   : Literal
    ///   | ParenExpr
    ///   | Callable   ArgumentExprList
    ///   | IDENT      ArgumentExprList?
    ///   | BITCAST    LESS_THAN TypeSpecifier GREATER_THAN ParenExpr
    pub fn primaryExpr(self: *Parser) !?Ast.Index(Ast.Expression) {
        if (try self.callable()) |call| {
            const args = try self.expectArgumentExprList();
            return try self.addExpr(.{ .call = .{ .callable = call, .args = args } });
        }

        const token = self.current_token;
        switch (token.tag) {
            .keyword_true => {
                _ = self.next();
                return try self.addExpr(.{ .literal = .{ .bool = true } });
            },
            .keyword_false => {
                _ = self.next();
                return try self.addExpr(.{ .literal = .{ .bool = false } });
            },
            .number => {
                _ = self.next();
                return try self.addExpr(.{ .literal = .{
                    .number = numberLiteral(token.loc.asStr(self.source)) catch unreachable,
                } });
            },
            .keyword_bitcast => {
                _ = self.next();
                _ = try self.expectToken(.less_than);
                const dest_type = try self.expectTypeSpecifier();
                _ = try self.expectToken(.greater_than);
                const args = try self.expectParenExpr();
                return try self.addExpr(.{ .bitcast = .{ .dest = dest_type, .expr = args } });
            },
            .paren_left => return try self.expectParenExpr(),
            .ident => {
                _ = self.next();
                if (self.current_token.tag == .paren_left) {
                    const args = try self.expectArgumentExprList();
                    return try self.addExpr(.{ .call = .{
                        .callable = .{ .ident = token.loc.asStr(self.source) },
                        .args = args,
                    } });
                }
                return try self.addExpr(.{ .ident = token.loc.asStr(self.source) });
            },
            else => return null,
        }
    }

    /// ParenExpr : PAREN_LEFT Expr PAREN_RIGHT
    pub fn expectParenExpr(self: *Parser) !Ast.Index(Ast.Expression) {
        _ = try self.expectToken(.paren_left);
        const expr = try self.expression() orelse {
            self.addError(
                self.current_token.loc,
                "unable to parse expression '{s}'",
                .{self.current_token.tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
        _ = try self.expectToken(.paren_right);
        return expr;
    }

    /// Callable
    ///   : TypeSpecifierWithoutIdent
    ///   | ARRAY
    ///   | MatrixPrefix
    ///   | VectorPrefix
    pub fn callable(self: *Parser) !?Ast.CallExpr.Callable {
        if (self.peek().tag != .less_than) {
            if (self.vectorPrefix()) |vec| return .{ .partial_vector = vec };
            if (self.matrixPrefix()) |mat| return .{ .partial_matrix = mat };
            if (self.current_token.tag == .keyword_array) {
                _ = self.next();
                return .partial_array;
            }
        }

        const type_token = self.current_token;
        const constructor = try self.typeSpecifierWithoutIdent() orelse return null;
        switch (self.ast.getType(constructor)) {
            .scalar => |p| return .{ .scalar = p },
            .vector => |p| return .{ .vector = p },
            .matrix => |p| return .{ .matrix = p },
            .array => |p| return .{ .array = p },
            else => {
                self.addError(
                    type_token.loc,
                    "type '{s}' can't be called (not expression)",
                    .{type_token.tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            },
        }
    }

    /// ArgumentExprList : PAREN_LEFT ((Expr COMMA)* Expr COMMA?)? PAREN_RIGHT
    pub fn expectArgumentExprList(self: *Parser) !Ast.Range(Ast.Index(Ast.Expression)) {
        var args = std.BoundedArray(Ast.Index(Ast.Expression), max_call_args).init(0) catch unreachable;
        _ = try self.expectToken(.paren_left);
        while (true) {
            const expr_token = self.current_token;
            const expr = try self.expression() orelse break;
            args.append(expr) catch {
                self.addError(
                    expr_token.loc,
                    "exceeded maximum call arguments ({})",
                    .{max_call_args},
                    &.{},
                );
                return error.Parsing;
            };

            if (self.eatToken(.comma) == null) break;
        }
        _ = try self.expectToken(.paren_right);

        try self.ast.extra.appendSlice(self.allocator, args.slice());
        return .{
            .start = @intCast(u32, self.ast.extra.items.len - args.len),
            .end = @intCast(u32, self.ast.extra.items.len),
        };
    }

    /// ElementCountExpr
    ///   : UnaryExpr MathExpr
    ///   | UnaryExpr BitwiseExpr
    pub fn elementCountExpr(self: *Parser) !?Ast.Index(Ast.Expression) {
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
    pub fn unaryExpr(self: *Parser) error{ OutOfMemory, Parsing }!?Ast.Index(Ast.Expression) {
        const op_token = self.current_token;
        const op: Ast.UnaryExpr.Operator = switch (op_token.tag) {
            .bang, .tilde => .not,
            .minus => .negate,
            .star => .deref,
            .@"and" => .addr_of,
            else => return self.singularExpr(),
        };
        _ = self.next();

        const expr = try self.unaryExpr() orelse {
            self.addError(
                self.current_token.loc,
                "unable to parse right side of '{s}' expression",
                .{op_token.tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };

        return try self.addExpr(.{ .unary = .{ .op = op, .expr = expr } });
    }

    /// SingularExpr : PrimaryExpr PostfixExpr TODO
    pub fn singularExpr(self: *Parser) !?Ast.Index(Ast.Expression) {
        return self.primaryExpr();
        // TODO: component_or_swizzle_specifier
    }

    /// MultiplicativeExpr : UnaryExpr | (STAR | DIVISION | MOD MultiplicativeExpr)*
    ///
    /// expects first expression ( UnaryExpr )
    pub fn expectMultiplicativeExpr(self: *Parser, left_unary: Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        var left = left_unary;
        while (true) {
            const op_token = self.current_token;
            const op: Ast.BinaryExpr.Operator = switch (op_token.tag) {
                .star => .multiply,
                .division => .divide,
                .mod => .modulo,
                else => return left,
            };
            _ = self.next();
            const right = try self.unaryExpr() orelse {
                self.addError(
                    self.current_token.loc,
                    "unable to parse right side of '{s}' expression",
                    .{op_token.tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            };
            left = try self.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
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
    pub fn expectAdditiveExpr(self: *Parser, left_mul: Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        var left = left_mul;
        while (true) {
            const op_token = self.current_token;
            const op: Ast.BinaryExpr.Operator = switch (op_token.tag) {
                .plus => .add,
                .minus => .subtract,
                else => return left,
            };
            _ = self.next();
            const unary = try self.unaryExpr() orelse {
                self.addError(
                    self.current_token.loc,
                    "unable to parse right side of '{s}' expression",
                    .{op_token.tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            };
            const right = try self.expectMultiplicativeExpr(unary);
            left = try self.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
        }
    }

    /// MathExpr : MultiplicativeExpr AdditiveExpr
    pub fn expectMathExpr(self: *Parser, left: Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        const right = try self.expectMultiplicativeExpr(left);
        return self.expectAdditiveExpr(right);
    }

    /// ShiftExpr
    ///   : MathExpr
    ///   | UnaryExpr SHIFT_LEFT  UnaryExpr
    ///   | UnaryExpr SHIFT_RIGHT UnaryExpr
    ///
    /// expects first expression ( UnaryExpr )
    pub fn expectShiftExpr(self: *Parser, left: Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operator = switch (op_token.tag) {
            .shift_left => .shift_left,
            .shift_right => .shift_right,
            else => return try self.expectMathExpr(left),
        };
        _ = self.next();

        const right = try self.unaryExpr() orelse {
            self.addError(
                self.current_token.loc,
                "unable to parse right side of '{s}' expression",
                .{op_token.tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };

        return try self.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
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
    pub fn expectRelationalExpr(self: *Parser, left_unary: Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        const left = try self.expectShiftExpr(left_unary);
        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operator = switch (op_token.tag) {
            .equal_equal => .equal,
            .not_equal => .not_equal,
            .less_than => .less,
            .less_than_equal => .less_equal,
            .greater_than => .greater,
            .greater_than_equal => .greater_equal,
            else => return left,
        };
        _ = self.next();

        const right_unary = try self.unaryExpr() orelse {
            self.addError(
                self.current_token.loc,
                "unable to parse right side of '{s}' expression",
                .{op_token.tag.symbol()},
                &.{},
            );
            return error.Parsing;
        };
        const right = try self.expectShiftExpr(right_unary);
        return try self.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
    }

    /// BitwiseExpr
    ///   : UnaryExpr AND UnaryExpr (AND UnaryExpr)*
    ///   | UnaryExpr OR  UnaryExpr (OR UnaryExpr)*
    ///   | UnaryExpr XOR UnaryExpr (XOR UnaryExpr)*
    ///
    /// expects first expression ( UnaryExpr )
    pub fn bitwiseExpr(self: *Parser, left: Ast.Index(Ast.Expression)) !?Ast.Index(Ast.Expression) {
        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operator = switch (op_token.tag) {
            .@"and" => .binary_and,
            .@"or" => .binary_or,
            .xor => .binary_xor,
            else => return null,
        };
        _ = self.next();

        var left_result = left;
        while (true) {
            const right = try self.unaryExpr() orelse {
                self.addError(
                    self.current_token.loc,
                    "unable to parse right side of '{s}' expression",
                    .{op_token.tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            };

            left_result = try self.addExpr(.{ .binary = .{ .op = op, .left = left_result, .right = right } });

            if (self.current_token.tag != op_token.tag) return left_result;
        }
    }

    /// ShortCircuitExpr
    ///   : RelationalExpr
    ///   | RelationalExpr (AND_AND RelationalExpr)*
    ///   | RelationalExpr (OR_OR   RelationalExpr)*
    ///
    /// expects first expression ( UnaryExpr )
    pub fn expectShortCircuitExpr(self: *Parser, left_relational: Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        var left = left_relational;

        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operator = switch (op_token.tag) {
            .and_and => .circuit_and,
            .or_or => .circuit_or,
            else => return left,
        };

        while (self.current_token.tag == op_token.tag) {
            _ = self.next();

            const right_unary = try self.unaryExpr() orelse {
                self.addError(
                    self.current_token.loc,
                    "unable to parse right side of '{s}' expression",
                    .{op_token.tag.symbol()},
                    &.{},
                );
                return error.Parsing;
            };
            const right = try self.expectRelationalExpr(right_unary);

            left = try self.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
        }

        return left;
    }

    /// Expr
    ///   : RelationalExpr
    ///   | BitwiseExpr
    ///   | RelationalExpr AND_AND RelationalExpr
    ///   | RelationalExpr OR_OR   RelationalExpr
    pub fn expression(self: *Parser) !?Ast.Index(Ast.Expression) {
        const left_unary = try self.unaryExpr() orelse return null;
        if (try self.bitwiseExpr(left_unary)) |bitwise| return bitwise;
        const left = try self.expectRelationalExpr(left_unary);
        return try self.expectShortCircuitExpr(left);
    }

    pub fn expectToken(self: *Parser, tag: Token.Tag) !Token {
        const token = self.next();

        if (token.tag == tag) return token;

        self.addError(
            token.loc,
            "expected '{s}', but found '{s}'",
            .{ tag.symbol(), token.tag.symbol() },
            &.{},
        );
        return error.Parsing;
    }

    pub fn eatToken(self: *Parser, tag: Token.Tag) ?Token {
        return if (self.current_token.tag == tag) self.next() else null;
    }

    pub fn peek(self: *Parser) Token {
        return self.tokenizer.peek();
    }

    pub fn next(self: *Parser) Token {
        const current = self.current_token;
        self.current_token = self.tokenizer.next();
        return current;
    }

    pub fn continueUntilOrEOF(self: *Parser, until: Token.Tag) void {
        while (true) {
            const tag = self.next().tag;
            if (tag == until or tag == .eof) break;
        }
    }

    pub fn addExpr(self: *Parser, expr: Ast.Expression) std.mem.Allocator.Error!Ast.Index(Ast.Expression) {
        const i = @intCast(u32, self.ast.expressions.items.len);
        try self.ast.expressions.append(self.allocator, expr);
        return i;
    }

    pub fn addGlobal(self: *Parser, decl: Ast.GlobalDecl) std.mem.Allocator.Error!void {
        return self.ast.globals.append(self.allocator, decl);
    }

    pub fn addType(self: *Parser, t: Ast.Type) std.mem.Allocator.Error!Ast.Index(Ast.Type) {
        const i = @intCast(u32, self.ast.types.items.len);
        try self.ast.types.append(self.allocator, t);
        return i;
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

    // TODO
    pub fn numberLiteral(str: []const u8) !Ast.Literal.Number {
        return .{ .abstract_int = try std.fmt.parseInt(i64, str, 10) };
    }
};

const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

test Parser {
    std.testing.refAllDeclsRecursive(Parser);
}

test "no errors" {
    const source =
        \\;
        \\@interpolate(flat) var expr = vec3<f32>(vec2(1, 5), 3);
        \\var<storage> expr = bitcast<f32>(5);
        \\var expr;
        \\var expr = bool();
        \\var expr = ~(-(!false));
        \\var expr = expr;
        \\var expr = expr(expr);
        \\const hello = 1;
        \\override hello;
        \\type the_type = ptr<workgroup, f32, read>;
        \\type the_type = array<f32, expr>;
        \\type the_type = vec3<f32>;
        \\type the_type = mat2x3<f32>;
        \\type the_type = atomic<u32>;
        \\type the_type = sampler;
        \\struct S {
        \\  s: u32,
        \\}
        \\const_assert 2 > 1;
    ;

    var ast = try parse(std.testing.allocator, source, null);
    defer ast.deinit(std.testing.allocator);
}

test "variable & expressions" {
    const source =
        \\var expr = 1 + 5 + 2 * 3 > 6 >> 7;
    ;

    var ast = try parse(std.testing.allocator, source, null);
    defer ast.deinit(std.testing.allocator);

    const expr = ast.getExpr(ast.getGlobal(0).variable.value.?).binary;

    const expr_left = ast.getExpr(expr.left).binary;
    const expr_left_left = ast.getExpr(expr_left.left).binary;
    const expr_left_left_left = ast.getExpr(expr_left_left.left).literal;
    const expr_left_left_right = ast.getExpr(expr_left_left.right).literal;
    const expr_left_right = ast.getExpr(expr_left.right).binary;
    const expr_left_right_left = ast.getExpr(expr_left_right.left).literal;
    const expr_left_right_right = ast.getExpr(expr_left_right.right).literal;

    const expr_right = ast.getExpr(expr.right).binary;
    const expr_right_left = ast.getExpr(expr_right.left).literal;
    const expr_right_right = ast.getExpr(expr_right.right).literal;

    try expect(expr_left.op == .add);
    try expect(expr_right.op == .shift_right);
    try expect(expr_left_left.op == .add);
    try expect(expr_left_right.op == .multiply);
    try expect(expr_left_left_left.number.abstract_int == 1);
    try expect(expr_left_left_right.number.abstract_int == 5);
    try expect(expr_left_right_left.number.abstract_int == 2);
    try expect(expr_left_right_right.number.abstract_int == 3);
    try expect(expr_right_left.number.abstract_int == 6);
    try expect(expr_right_right.number.abstract_int == 7);
}

test "type alias" {
    const source =
        \\type my_type = vec3<f32>;
    ;

    var ast = try parse(std.testing.allocator, source, null);
    defer ast.deinit(std.testing.allocator);

    const vec3 = ast.getType(ast.getGlobal(0).type_alias.type);
    const vec3_elements_type = ast.getType(vec3.vector.element);

    try expect(vec3.vector.prefix == .vec3);
    try expect(vec3_elements_type.scalar == .f32);
}

test "struct" {
    const source =
        \\struct S { s: u32 }
    ;

    var ast = try parse(std.testing.allocator, source, null);
    defer ast.deinit(std.testing.allocator);

    const strct = ast.getGlobal(0).@"struct";
    const strct_member_s = strct.members[0];
    const strct_member_s_type = ast.getType(strct_member_s.type).scalar;

    try expect(strct.members.len == 1);
    try expectEqualStrings("S", strct.name);
    try expectEqualStrings("s", strct_member_s.name);
    try expect(strct_member_s_type == .u32);
}

test "research" {
    if (true) return error.SkipZigTest;

    const source =
        \\type my_type = array<f32, sampler()>;
    ;

    var ast = try parse(std.testing.allocator, source, null);
    defer ast.deinit(std.testing.allocator);

    const vec3 = ast.getType(ast.getGlobal(0).type_alias.type);
    const vec3_elements_type = ast.getType(vec3.vector.element);

    try expect(vec3.vector.prefix == .vec3);
    try expect(vec3_elements_type.scalar == .f32);
}
