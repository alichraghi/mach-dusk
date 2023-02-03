const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");
const Tokenizer = @import("Tokenizer.zig");

pub fn parse(
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    options: struct { emit_errors: bool = true },
) !Ast {
    var tokenizer = Tokenizer.init(source);
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();
    while (true) {
        const tok = tokenizer.next();
        try tokens.append(tok);
        if (tok.tag == .eof) break;
    }

    var parser = Parser{
        .allocator = allocator,
        .source = source,
        .tokens = tokens.items,
        .tok_i = 0,
        .errors = .{},
        .ast = .{},
    };
    try parser.parseRoot();
    if (parser.errors.items.len > 0) {
        defer parser.deinit();
        if (options.emit_errors) try parser.emitErrors();
        return error.Parsing;
    }

    return parser.ast;
}

const Parser = struct {
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    tokens: []const Token,
    tok_i: u32,
    errors: std.ArrayListUnmanaged(Error),
    ast: Ast,

    pub fn deinit(self: *Parser) void {
        self.errors.deinit(self.allocator);
        self.ast.deinit(self.allocator);
    }

    pub const Error = struct {
        tag: Tag,
        token: Token,
        notes: []const []const u8 = &.{},

        pub const Tag = union(enum) {
            expected_token: Token.Tag,
            expected_expr,
            expected_type_decl,
            expected_scalar_type,
            expected_float_type,
        };
    };

    /// print beauty errors to stderr
    pub fn emitErrors(self: Parser) !void {
        const stderr = std.io.getStdErr().writer();
        var bw = std.io.bufferedWriter(stderr);
        const bw_writer = bw.writer();
        const cfg = std.debug.TTY.Config{ .escape_codes = {} };

        for (self.errors.items) |err| {
            const loc_extra = err.token.loc.extraInfo(self.source);

            // file:line:column
            try cfg.setColor(bw_writer, .Bold);
            try bw_writer.print(":{d}:{d} ", .{ loc_extra.line, loc_extra.col });

            try cfg.setColor(bw_writer, .Red);
            try bw_writer.writeAll("error: ");

            // error message
            try cfg.setColor(bw_writer, .Reset);
            try cfg.setColor(bw_writer, .Bold);
            switch (err.tag) {
                .expected_token => |expected_token| {
                    try bw_writer.print("expected '{s}', but found '{s}'", .{
                        expected_token.symbol(),
                        err.token.tag.symbol(),
                    });
                },
                .expected_expr => {
                    try bw_writer.print("expected expression, but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
                .expected_type_decl => {
                    try bw_writer.print("expected type declaration, but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
                .expected_scalar_type => {
                    try bw_writer.print("expected an scalar type ('i32', 'u32', 'f32', 'f16' or 'bool'), but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
                .expected_float_type => {
                    try bw_writer.print("expected a floating-point type ('f32' or 'f16'), but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
            }
            try bw_writer.writeByte('\n');

            // error line
            try cfg.setColor(bw_writer, .Reset);
            try bw_writer.writeAll(self.source[loc_extra.line_start..loc_extra.line_end]);
            try bw_writer.writeByte('\n');

            // error location pointer ('^')
            try bw_writer.writeByteNTimes(' ', loc_extra.col - 1);
            try cfg.setColor(bw_writer, .Bold);
            try cfg.setColor(bw_writer, .Green);
            try bw_writer.writeByte('^');
            try bw_writer.writeByte('\n');
            try cfg.setColor(bw_writer, .Reset);
        }

        try bw.flush();
    }

    pub fn parseRoot(self: *Parser) !void {
        self.tok_i = 0;
        self.errors.clearAndFree(self.allocator);

        while (true) {
            const token = self.nextToken();
            switch (token.tag) {
                .keyword_type => {
                    const res = self.parseTypeAlias() catch |err| switch (err) {
                        error.Parsing => continue,
                        else => return err,
                    };
                    _ = try self.addGlobalDecl(.{ .type_alias = res });
                },
                .eof => break,
                else => {},
            }
        }
    }

    /// TODO
    fn parseExpr(self: *Parser) !Ast.Index(Ast.Expression) {
        const token = self.nextToken();
        switch (token.tag) {
            .keyword_true => return self.addExpr(.{ .literal = .{ .bool = true } }),
            .keyword_false => return self.addExpr(.{ .literal = .{ .bool = false } }),
            .number => return self.addExpr(.{
                .literal = .{
                    .number = createNumber(token.loc.asStr(self.source)) catch unreachable, // TODO
                },
            }),
            .keyword_vec2,
            .keyword_vec3,
            .keyword_vec4,
            => {
                if (self.eatToken(.less_than)) |_| {
                    const elem_type = try self.parseScalarType();
                    _ = try self.expectToken(.greater_than);
                    const args = try self.parseArguments();

                    return self.addExpr(.{ .construct = .{
                        .type = .{
                            .vector = .{ .size = tokenToVectorSize(token.tag), .element_type = elem_type },
                        },
                        .components = args,
                    } });
                } else {
                    const args = try self.parseArguments();

                    return self.addExpr(.{ .construct = .{
                        .type = .{
                            .partial_vector = .{ .size = tokenToVectorSize(token.tag) },
                        },
                        .components = args,
                    } });
                }
            },
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
                unreachable;
                // return .{ .construct = .{ .type = try self.parseConstructorTypeOrError(), .components = undefined } };
            },
            else => {
                try self.addError(.{
                    .token = token,
                    .tag = .{ .expected_expr = {} },
                });
                return error.Parsing;
            },
        }
    }

    /// Arguments <- LEFT_PAREN (Expr COMMA?)* RIGHT_PAREN
    fn parseArguments(self: *Parser) error{ Parsing, OutOfMemory }!Ast.Range(Ast.Index(Ast.Expression)) {
        _ = try self.expectToken(.paren_left);

        var args = std.ArrayList(Ast.Index(Ast.Expression)).init(self.allocator);
        defer args.deinit();

        while (true) {
            try args.append(try self.parseExpr());
            const token = self.nextToken();
            switch (token.tag) {
                .comma => {},
                .paren_right => break,
                else => {
                    try self.addError(.{
                        .token = token,
                        .tag = .{ .expected_token = .paren_right },
                    });
                    return error.Parsing;
                },
            }
        }

        try self.ast.expressions_extra.appendSlice(self.allocator, args.items);
        return .{
            .start = self.ast.expressions_extra.items.len - args.items.len,
            .end = self.ast.expressions_extra.items.len,
        };
    }

    /// TypeAlias <- KEYWORD_type IDENTIFIER EQUAL TypeDecl
    fn parseTypeAlias(self: *Parser) !Ast.TypeAlias {
        const name = try self.expectToken(.identifier);
        _ = try self.expectToken(.equal);
        const value = try self.parseType();
        _ = try self.expectToken(.semicolon);
        return .{ .name = name.loc.asStr(self.source), .type = value };
    }

    /// Type
    ///     <- ScalarType
    ///      / VectorType
    ///      / MatrixType
    ///      / AtomicType
    ///      / ArrayType
    ///--------------------------------------------------------------------------------
    /// AtomicType <- KEYWORD_atomic LESS_THAN KEYWORD_i32 / KEYWORD_u32 GREATER_THAN
    ///--------------------------------------------------------------------------------
    /// ArrayType
    ///     <- KEYWORD_array LESS_THAN
    ///        ScalarType
    ///      / VectorType
    ///      / MatrixType
    ///      / AtomicType
    ///      / ArrayType
    ///      / StructType GREATER_THAN
    ///
    /// NOTE: ArrayType and StructType elements must have a creation-fixed footprint
    ///--------------------------------------------------------------------------------
    /// VectorType
    ///     <- KEYWORD_vec2
    ///      / KEYWORD_vec3
    ///      / KEYWORD_vec4
    ///     LESS_THAN ScalarType GREATER_THAN
    ///--------------------------------------------------------------------------------
    /// MatrixType
    ///     <- KEYWORD_mat2x2
    ///      / KEYWORD_mat2x3
    ///      / KEYWORD_mat2x4
    ///      / KEYWORD_mat3x2
    ///      / KEYWORD_mat3x3
    ///      / KEYWORD_mat3x4
    ///      / KEYWORD_mat4x2
    ///      / KEYWORD_mat4x3
    ///      / KEYWORD_mat4x4
    ///     LESS_THAN KEYWORD_f32 / KEYWORD_f16 GREATER_THAN
    ///--------------------------------------------------------------------------------
    /// UserType <- IDENTIFIER
    ///
    /// TODO: separate parse function for each type
    fn parseType(self: *Parser) !Ast.Type {
        const token = self.nextToken();
        switch (token.tag) {
            .keyword_i32,
            .keyword_u32,
            .keyword_f32,
            .keyword_f16,
            .keyword_bool,
            => {
                return .{ .scalar = tokenToScalarType(token.tag) };
            },
            .keyword_atomic => {
                _ = try self.expectToken(.less_than);
                const elem_type = try self.parseScalarType();
                _ = try self.expectToken(.greater_than);
                return .{ .atomic = .{ .element_type = elem_type } };
            },
            .keyword_sampler => {
                return .{ .sampler = .{ .comparison = false } };
            },
            .keyword_comparison_sampler => {
                return .{ .sampler = .{ .comparison = true } };
            },
            .keyword_array => {
                _ = try self.expectToken(.less_than);
                const elem_type = self.ast.types.items.len;
                try self.ast.types.append(self.allocator, try self.parseType());

                // dynamic size
                _ = self.eatToken(.comma) orelse {
                    _ = try self.expectToken(.greater_than);
                    return .{ .array = .{ .element_type = elem_type, .size = .dynamic } };
                };

                // constant size
                const size = try self.parseExpr();
                _ = try self.expectToken(.greater_than);
                return .{ .array = .{ .element_type = elem_type, .size = .{ .constant = size } } };
            },
            .keyword_vec2, .keyword_vec3, .keyword_vec4 => {
                _ = try self.expectToken(.less_than);
                const elem_type = try self.parseScalarType();
                _ = try self.expectToken(.greater_than);
                return .{ .vector = .{ .element_type = elem_type, .size = tokenToVectorSize(token.tag) } };
            },
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
                _ = try self.expectToken(.less_than);

                const elem_type_token = self.eatToken(.keyword_f16) orelse
                    self.eatToken(.keyword_f32) orelse {
                    try self.addError(.{
                        .token = token,
                        .tag = .{ .expected_float_type = {} },
                    });
                    return error.Parsing;
                };
                const elem_type = tokenToScalarType(elem_type_token.tag);

                _ = try self.expectToken(.greater_than);
                return .{ .matrix = .{
                    .element_type = elem_type,
                    .rows = tokenToMatrixRows(token.tag),
                    .columns = tokenToMatrixColumns(token.tag),
                } };
            },
            .identifier => {
                return .{ .user = token.loc.asStr(self.source) };
            },
            else => {
                try self.addError(.{
                    .token = token,
                    .tag = .{ .expected_type_decl = {} },
                });
                return error.Parsing;
            },
        }
    }

    /// ScalarType
    ///     <- KEYWORD_i32
    ///      / KEYWORD_u32
    ///      / KEYWORD_f32
    ///      / KEYWORD_f16
    ///      / KEYWORD_bool
    fn parseScalarType(self: *Parser) !Ast.ScalarType {
        const token = self.nextToken();
        switch (token.tag) {
            .keyword_i32,
            .keyword_u32,
            .keyword_f16,
            .keyword_f32,
            .keyword_bool,
            => return tokenToScalarType(token.tag),
            else => {
                try self.addError(.{
                    .token = token,
                    .tag = .{ .expected_scalar_type = {} },
                });
                return error.Parsing;
            },
        }
    }

    fn expectToken(self: *Parser, tag: Token.Tag) !Token {
        if (self.tokens[self.tok_i].tag == tag) {
            return self.nextToken();
        } else {
            try self.addError(.{
                .token = self.tokens[self.tok_i],
                .tag = .{ .expected_token = tag },
            });
            return error.Parsing;
        }
    }

    fn eatToken(self: *Parser, tag: Token.Tag) ?Token {
        return if (self.tokens[self.tok_i].tag == tag) self.nextToken() else null;
    }

    fn nextToken(self: *Parser) Token {
        const current = self.tokens[self.tok_i];
        self.tok_i += 1;
        return current;
    }

    fn tokenToScalarType(tag: Token.Tag) Ast.ScalarType {
        return switch (tag) {
            .keyword_i32 => .{ .kind = .int, .width = 4 },
            .keyword_u32 => .{ .kind = .uint, .width = 4 },
            .keyword_f32 => .{ .kind = .float, .width = 4 },
            .keyword_f16 => .{ .kind = .float, .width = 2 },
            .keyword_bool => .{ .kind = .bool, .width = 1 },
            else => unreachable,
        };
    }

    fn tokenToVectorSize(tag: Token.Tag) Ast.VectorType.Size {
        return switch (tag) {
            .keyword_vec2 => .bi,
            .keyword_vec3 => .tri,
            .keyword_vec4 => .quad,
            else => unreachable,
        };
    }

    fn tokenToMatrixColumns(tag: Token.Tag) Ast.VectorType.Size {
        return switch (tag) {
            .keyword_mat2x2, .keyword_mat2x3, .keyword_mat2x4 => .bi,
            .keyword_mat3x2, .keyword_mat3x3, .keyword_mat3x4 => .tri,
            .keyword_mat4x2, .keyword_mat4x3, .keyword_mat4x4 => .quad,
            else => unreachable,
        };
    }

    fn tokenToMatrixRows(tag: Token.Tag) Ast.VectorType.Size {
        return switch (tag) {
            .keyword_mat2x2, .keyword_mat3x2, .keyword_mat4x2 => .bi,
            .keyword_mat2x3, .keyword_mat3x3, .keyword_mat4x3 => .tri,
            .keyword_mat2x4, .keyword_mat3x4, .keyword_mat4x4 => .quad,
            else => unreachable,
        };
    }

    fn addError(self: *Parser, err: Error) std.mem.Allocator.Error!void {
        try self.errors.append(self.allocator, err);
    }

    fn addExpr(self: *Parser, expr: Ast.Expression) std.mem.Allocator.Error!Ast.Index(Ast.Expression) {
        const i = self.ast.expressions.items.len;
        try self.ast.expressions.append(self.allocator, expr);
        return i;
    }

    fn addGlobalDecl(self: *Parser, decl: Ast.GlobalDecl) std.mem.Allocator.Error!Ast.Index(Ast.GlobalDecl) {
        const i = self.ast.globals.items.len;
        try self.ast.globals.append(self.allocator, decl);
        return i;
    }
};

// TODO
pub fn createNumber(str: []const u8) !Ast.Number {
    return .{
        .abstract_int = try std.fmt.parseInt(
            i64,
            str,
            10,
        ),
    };
}

test {
    const str =
        \\type t1 = array<i32, vec3(1, 2, 3)>;
    ;
    var p = parse(std.testing.allocator, str, .{}) catch return;
    defer p.deinit(std.testing.allocator);

    const t1 = p.globals.items[0].type_alias;
    const array_i32 = t1.type.array;
    const array_i32_elem_type = p.types.items[array_i32.element_type];
    try std.testing.expectEqual(Ast.Type{ .scalar = .{ .kind = .int, .width = 4 } }, array_i32_elem_type);

    const vec3_expr = p.expressions.items[array_i32.size.constant].construct;
    const vec3_comps = p.expressions_extra.items[vec3_expr.components.start..vec3_expr.components.end];
    try std.testing.expectEqual(
        Ast.ConstructorType{ .partial_vector = .{ .size = .tri } },
        vec3_expr.type,
    );
    try std.testing.expectEqual(@as(usize, 3), vec3_comps.len);
    try std.testing.expectEqual(
        Ast.Expression{ .literal = .{ .number = .{ .abstract_int = 1 } } },
        p.expressions.items[vec3_comps[0]],
    );
    try std.testing.expectEqual(
        Ast.Expression{ .literal = .{ .number = .{ .abstract_int = 2 } } },
        p.expressions.items[vec3_comps[1]],
    );
    try std.testing.expectEqual(
        Ast.Expression{ .literal = .{ .number = .{ .abstract_int = 3 } } },
        p.expressions.items[vec3_comps[2]],
    );
}
