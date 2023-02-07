const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");
const Tokenizer = @import("Tokenizer.zig");

const max_call_args = 64;

pub fn parse(
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    options: struct { emit_errors: bool = true },
) !Ast {
    var tokenizer = Tokenizer.init(source);
    var first_token = tokenizer.next();

    var parser = Parser{
        .allocator = allocator,
        .source = source,
        .tokenizer = tokenizer,
        .current_token = first_token,
        .errors = .{},
        .ast = .{},
    };
    errdefer {
        parser.errors.deinit(allocator);
        parser.ast.deinit(allocator);
    }

    try parser.ast.globals.ensureTotalCapacityPrecise(allocator, source.len / 50); // 1:50 source to globals
    try parser.ast.expressions.ensureTotalCapacityPrecise(allocator, source.len / 5); // 1:5 source to expressions

    while (true) {
        const token = parser.current_token;
        switch (token.tag) {
            .keyword_type => {
                const res = parser.parseTypeAlias() catch |err| switch (err) {
                    error.Parsing => {
                        while (parser.nextToken().tag != .semicolon) {}
                        continue;
                    },
                    else => return err,
                };
                _ = try parser.addGlobalDecl(.{ .type_alias = res });
            },
            .eof => break,
            else => {
                std.debug.print("Unsupported global\n", .{});
            },
        }
    }

    if (parser.errors.items.len > 0) {
        if (options.emit_errors) try parser.emitErrors();
        return error.Parsing;
    }

    return parser.ast;
}

const Parser = struct {
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    tokenizer: Tokenizer,
    current_token: Token,
    errors: std.ArrayListUnmanaged(Error),
    ast: Ast,

    pub const Error = struct {
        tag: Tag,
        token: Token,
        notes: []const []const u8 = &.{},

        pub const Tag = union(enum) {
            expected_token: Token.Tag,
            expected_expr,
            expected_literal_expr,
            expected_construct_expr,
            expected_bitcast_expr,
            expected_type,
            expected_scalar_type,
            expected_float_type,
            expected_sampler_type,
            expected_vector_type,
            expected_matrix_type,
            expected_atomic_type,
            expected_array_type,
            expected_at_least_x_args: u8,
            exceeded_max_args: u8,
            invalid_bitcast_dest_type,
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
                .expected_literal_expr => {
                    try bw_writer.print("expected literal expression, but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
                .expected_construct_expr => {
                    try bw_writer.print("expected construct expression ( e.g vec2(1.0, 2.0) ), but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
                .expected_bitcast_expr => {
                    try bw_writer.print("expected bitcast expression, but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
                .expected_type => {
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
                .expected_sampler_type => {
                    try bw_writer.print("expected a sampler type ('sampler' or 'sampler_comparison'), but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
                .expected_vector_type => {
                    try bw_writer.print("expected a vector type ('vec2<T>', 'vec3<T>' or 'vec4<T>'), but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
                .expected_matrix_type => {
                    try bw_writer.print("expected a matrix type ('mat2x2<T>', 'mat2x3<T>', 'mat2x4<T>', 'mat3x2<T>', 'mat3x3<T>', 'mat3x4<T>', 'mat4x2<T>', 'mat4x3<T>' or 'mat4x4<T>'), but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
                .expected_atomic_type => {
                    try bw_writer.print("expected an atomic type ('atomic<T>'), but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
                .expected_array_type => {
                    try bw_writer.print("expected an array type ('array<T>'), but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
                .expected_at_least_x_args => |min| {
                    try bw_writer.print("expected at least {} args", .{min});
                },
                .exceeded_max_args => |max| {
                    try bw_writer.print("exceeded maximum call arguments number ({})", .{max});
                },
                .invalid_bitcast_dest_type => {
                    try bw_writer.print("invalid bitcast destination type. must be eathir ('i32', 'u32' or 'f32') but found '{s}'", .{
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

    /// Expression <- LiteralExpr | ConstructExpr
    fn parseExpr(self: *Parser) !Ast.Expression {
        const token = self.current_token;
        if (token.tag.isLiteral()) {
            return .{ .literal = try self.parseLiteralExpr() };
        } else if (token.tag.isScalarType() or
            token.tag.isVectorType() or
            token.tag.isMatrixType() or
            token.tag == .keyword_array)
        {
            return .{ .construct = try self.parseConstructExpr() };
        } else if (token.tag == .keyword_bitcast) {
            return .{ .bitcast = try self.parseBitcastExpr() };
        }

        try self.addError(.{
            .token = token,
            .tag = .{ .expected_expr = {} },
        });
        return error.Parsing;
    }

    /// LiteralExpr <- Number | KEYWORD_true | KEYWORD_false
    fn parseLiteralExpr(self: *Parser) !Ast.Literal {
        const token = self.nextToken();
        return switch (token.tag) {
            .keyword_true => .{ .bool = true },
            .keyword_false => .{ .bool = false },
            .number => .{ .number = parseNumberLiteral(token.loc.asStr(self.source)) catch unreachable },
            else => {
                try self.addError(.{
                    .token = token,
                    .tag = .{ .expected_literal_expr = {} },
                });
                return error.Parsing;
            },
        };
    }

    // TODO
    pub fn parseNumberLiteral(str: []const u8) !Ast.Literal.Number {
        return .{ .abstract_int = try std.fmt.parseInt(
            i64,
            str,
            10,
        ) };
    }

    /// ConstructExpr
    ///              <- VectorType
    ///               | MatrixType
    ///               | (KEYWORD_array | ArrayType)
    ///                 LEFT_PAREN ( Expression COMMA? )* RIGHT_PAREN
    fn parseConstructExpr(self: *Parser) !Ast.ConstructExpr {
        const token = self.current_token;
        if (token.tag.isScalarType()) {
            const scalar_type = try self.parseScalarType();
            const args = try self.parseCallArguments(0, max_call_args);
            return .{
                .type = .{
                    .scalar = scalar_type,
                },
                .components = args,
            };
        } else if (token.tag.isVectorType()) { // TODO max args
            const vector_type = try self.parseVectorType(false);
            const args = try self.parseCallArguments(0, max_call_args);
            return .{ .type = .{ .vector = vector_type }, .components = args };
        } else if (token.tag.isMatrixType()) {
            const matrix_type = try self.parseMatrixType(false);
            const args = try self.parseCallArguments(0, max_call_args);
            return .{ .type = .{ .matrix = matrix_type }, .components = args };
        } else if (token.tag == .keyword_array) {
            if (self.peekToken().tag == .less_than) {
                const array_type = try self.parseArrayType();
                const args = try self.parseCallArguments(0, max_call_args);
                return .{
                    .type = .{ .full_array = array_type },
                    .components = args,
                };
            }

            _ = self.nextToken();
            const args = try self.parseCallArguments(0, max_call_args);
            return .{
                .type = .{ .partial_array = {} },
                .components = args,
            };
        }

        try self.addError(.{
            .token = token,
            .tag = .{ .expected_construct_expr = {} },
        });
        return error.Parsing;
    }

    /// BitcastExpr
    ///              <- VectorType
    ///               | MatrixType
    ///               | (KEYWORD_array | ArrayType)
    ///                 LEFT_PAREN ( Expression COMMA? )* RIGHT_PAREN
    fn parseBitcastExpr(self: *Parser) !Ast.BitcastExpr {
        const token = self.nextToken();
        if (token.tag == .keyword_bitcast) {
            _ = try self.expectToken(.less_than);

            const scalar_type = try self.parseScalarType();
            switch (scalar_type) {
                .i32, .u32, .f32 => {},
                else => {
                    try self.addError(.{
                        .token = token,
                        .tag = .{ .invalid_bitcast_dest_type = {} },
                    });
                    return error.Parsing;
                },
            }

            _ = try self.expectToken(.greater_than);
            const args = try self.parseCallArguments(1, 1);
            return .{
                .dest = scalar_type,
                .expr = args.one,
            };
        }

        try self.addError(.{
            .token = token,
            .tag = .{ .expected_bitcast_expr = {} },
        });
        return error.Parsing;
    }

    /// CallArguments <- LEFT_PAREN (Expr COMMA?)* RIGHT_PAREN
    fn parseCallArguments(self: *Parser, min: u8, max: u8) error{ Parsing, OutOfMemory }!Ast.Span(Ast.Expression) {
        std.debug.assert(max <= max_call_args);

        _ = try self.expectToken(.paren_left);
        if (self.current_token.tag == .paren_right) {
            _ = self.nextToken();
            return .{ .zero = {} };
        }

        var args = std.BoundedArray(Ast.Expression, max_call_args).init(0) catch unreachable;
        while (true) {
            const expr_token = self.current_token;
            args.append(try self.parseExpr()) catch {
                try self.addError(.{
                    .token = expr_token,
                    .tag = .{ .exceeded_max_args = max_call_args },
                });
                return error.Parsing;
            };

            if (args.len > max) {
                try self.addError(.{
                    .token = expr_token,
                    .tag = .{ .exceeded_max_args = max },
                });
                return error.Parsing;
            }

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

        if (args.len < min) {
            try self.addError(.{
                .token = self.current_token,
                .tag = .{ .expected_at_least_x_args = min },
            });
            return error.Parsing;
        }

        if (args.len == 1) {
            try self.ast.expressions.append(self.allocator, args.get(0));
            return .{ .one = @intCast(u32, self.ast.expressions.items.len - 1) };
        } else {
            try self.ast.expressions.appendSlice(self.allocator, args.slice());
            return .{ .multi = .{
                .start = @intCast(u32, self.ast.expressions.items.len - args.len),
                .end = @intCast(u32, self.ast.expressions.items.len),
            } };
        }
    }

    /// TypeAlias <- KEYWORD_type IDENTIFIER EQUAL Type
    fn parseTypeAlias(self: *Parser) !Ast.TypeAlias {
        _ = self.nextToken();
        const name = try self.expectToken(.identifier);
        _ = try self.expectToken(.equal);
        const value = try self.parseType();
        _ = try self.expectToken(.semicolon);
        return .{ .name = name.loc.asStr(self.source), .type = value };
    }

    /// Type
    ///     <- ScalarType
    ///      | VectorType.Full
    ///      | MatrixType.Full
    ///      | AtomicType
    ///      | ArrayType
    ///      | IDENTIFIER
    fn parseType(self: *Parser) !Ast.PlainType {
        const token = self.current_token;
        if (token.tag.isScalarType()) {
            return .{ .scalar = try self.parseScalarType() };
        } else if (token.tag.isSamplerType()) {
            return .{ .sampler = try self.parseSamplerType() };
        } else if (token.tag.isVectorType()) {
            return .{ .vector = try self.parseVectorType(true) };
        } else if (token.tag.isMatrixType()) {
            return .{ .matrix = try self.parseMatrixType(true) };
        } else if (token.tag == .keyword_atomic) {
            return .{ .atomic = try self.parseAtomicType() };
        } else if (token.tag == .keyword_array) {
            return .{ .array = try self.parseArrayType() };
        } else if (token.tag == .identifier) {
            _ = self.nextToken();
            return .{ .user = token.loc.asStr(self.source) };
        }

        try self.addError(.{
            .token = token,
            .tag = .{ .expected_type = {} },
        });
        return error.Parsing;
    }

    /// ScalarType
    ///     <- KEYWORD_i32
    ///      | KEYWORD_u32
    ///      | KEYWORD_f32
    ///      | KEYWORD_f16
    ///      | KEYWORD_bool
    fn parseScalarType(self: *Parser) !Ast.ScalarType {
        const token = self.nextToken();
        return switch (token.tag) {
            .keyword_i32 => .i32,
            .keyword_u32 => .u32,
            .keyword_f32 => .f32,
            .keyword_f16 => .f16,
            .keyword_bool => .bool,
            else => {
                try self.addError(.{
                    .token = token,
                    .tag = .{ .expected_scalar_type = {} },
                });
                return error.Parsing;
            },
        };
    }

    /// SamplerType <- KEYWORD_sampler | KEYWORD_comparison_sampler
    pub fn parseSamplerType(self: *Parser) !Ast.SamplerType {
        const token = self.nextToken();
        return switch (token.tag) {
            .keyword_sampler => .{ .comparison = false },
            .keyword_comparison_sampler => .{ .comparison = true },
            else => {
                try self.addError(.{
                    .token = token,
                    .tag = .{ .expected_sampler_type = {} },
                });
                return error.Parsing;
            },
        };
    }

    /// VectorType <- VectorType.Full | VectorType.Partial
    ///
    /// VectorType.Full <- VectorType.Partial LESS_THAN ScalarType GREATER_THAN
    ///
    /// VectorType.Partial <- KEYWORD_vec2
    ///                     | KEYWORD_vec3
    ///                     | KEYWORD_vec4
    pub fn parseVectorType(self: *Parser, strict: bool) !Ast.VectorType {
        const token = self.nextToken();
        const size: Ast.VectorType.Size = switch (token.tag) {
            .keyword_vec2 => .bi,
            .keyword_vec3 => .tri,
            .keyword_vec4 => .quad,
            else => {
                try self.addError(.{
                    .token = token,
                    .tag = .{ .expected_vector_type = {} },
                });
                return error.Parsing;
            },
        };

        if (!strict and self.current_token.tag != .less_than) {
            return .{ .partial = .{ .size = size } };
        }

        _ = try self.expectToken(.less_than);
        const elem_type = try self.parseScalarType();
        _ = try self.expectToken(.greater_than);

        return .{ .full = .{ .size = size, .element_type = elem_type } };
    }

    /// MatrixType <- MatrixType.Full | MatrixType.Partial
    ///
    /// MatrixType.Full <- MatrixType.Partial LESS_THAN KEYWORD_f32 | KEYWORD_f16 GREATER_THAN
    ///
    /// MatrixType.Partial
    ///     <- KEYWORD_mat2x2
    ///      | KEYWORD_mat2x3
    ///      | KEYWORD_mat2x4
    ///      | KEYWORD_mat3x2
    ///      | KEYWORD_mat3x3
    ///      | KEYWORD_mat3x4
    ///      | KEYWORD_mat4x2
    ///      | KEYWORD_mat4x3
    ///      | KEYWORD_mat4x4
    pub fn parseMatrixType(self: *Parser, strict: bool) !Ast.MatrixType {
        const token = self.nextToken();
        if (!token.tag.isMatrixType()) {
            try self.addError(.{
                .token = token,
                .tag = .{ .expected_matrix_type = {} },
            });
            return error.Parsing;
        }

        const columns: Ast.VectorType.Size = switch (token.tag) {
            .keyword_mat2x2, .keyword_mat2x3, .keyword_mat2x4 => .bi,
            .keyword_mat3x2, .keyword_mat3x3, .keyword_mat3x4 => .tri,
            .keyword_mat4x2, .keyword_mat4x3, .keyword_mat4x4 => .quad,
            else => unreachable,
        };
        const rows: Ast.VectorType.Size = switch (token.tag) {
            .keyword_mat2x2, .keyword_mat3x2, .keyword_mat4x2 => .bi,
            .keyword_mat2x3, .keyword_mat3x3, .keyword_mat4x3 => .tri,
            .keyword_mat2x4, .keyword_mat3x4, .keyword_mat4x4 => .quad,
            else => unreachable,
        };

        if (!strict and self.current_token.tag != .less_than) {
            return .{ .partial = .{ .columns = columns, .rows = rows } };
        }

        _ = try self.expectToken(.less_than);
        const elem_type_token = self.nextToken();
        const elem_type: Ast.ScalarType = switch (elem_type_token.tag) {
            .keyword_f32 => .f32,
            .keyword_f16 => .f16,
            else => {
                try self.addError(.{
                    .token = elem_type_token,
                    .tag = .{ .expected_float_type = {} },
                });
                return error.Parsing;
            },
        };
        _ = try self.expectToken(.greater_than);

        return .{ .full = .{ .columns = columns, .rows = rows, .element_type = elem_type } };
    }

    /// AtomicType <- KEYWORD_atomic LESS_THAN KEYWORD_i32 | KEYWORD_u32 GREATER_THAN
    pub fn parseAtomicType(self: *Parser) !Ast.AtomicType {
        const token = self.nextToken();
        if (token.tag != .keyword_atomic) {
            try self.addError(.{
                .token = token,
                .tag = .{ .expected_atomic_type = {} },
            });
            return error.Parsing;
        }

        _ = try self.expectToken(.less_than);
        const elem_type = try self.parseScalarType();
        _ = try self.expectToken(.greater_than);
        return .{ .element_type = elem_type };
    }

    /// ArrayType
    ///     <- KEYWORD_array LESS_THAN
    ///        ScalarType
    ///      | VectorType
    ///      | MatrixType
    ///      | AtomicType
    ///      | ArrayType
    ///      | StructType GREATER_THAN
    ///
    /// NOTE: ArrayType and StructType elements must have a creation-fixed footprint
    pub fn parseArrayType(self: *Parser) error{ Parsing, OutOfMemory }!Ast.ArrayType {
        const token = self.nextToken();
        if (token.tag != .keyword_array) {
            try self.addError(.{
                .token = token,
                .tag = .{ .expected_array_type = {} },
            });
            return error.Parsing;
        }

        _ = try self.expectToken(.less_than);
        const elem_type = @intCast(u32, self.ast.types.items.len);
        try self.ast.types.append(self.allocator, try self.parseType());

        if (self.eatToken(.comma)) |_| {
            const size = try self.addExpr(try self.parseExpr());
            _ = try self.expectToken(.greater_than);
            return .{ .element_type = elem_type, .size = .{ .static = size } };
        }

        _ = try self.expectToken(.greater_than);
        return .{ .element_type = elem_type, .size = .dynamic };
    }

    fn expectToken(self: *Parser, tag: Token.Tag) !Token {
        if (self.current_token.tag == tag) {
            return self.nextToken();
        } else {
            try self.addError(.{
                .token = self.current_token,
                .tag = .{ .expected_token = tag },
            });
            return error.Parsing;
        }
    }

    fn eatToken(self: *Parser, tag: Token.Tag) ?Token {
        return if (self.current_token.tag == tag) self.nextToken() else null;
    }

    fn peekToken(self: *Parser) Token {
        return self.tokenizer.peek();
    }

    fn nextToken(self: *Parser) Token {
        const current = self.current_token;
        self.current_token = self.tokenizer.next();
        return current;
    }

    fn addError(self: *Parser, err: Error) std.mem.Allocator.Error!void {
        try self.errors.append(self.allocator, err);
    }

    fn addExpr(self: *Parser, expr: Ast.Expression) std.mem.Allocator.Error!Ast.Index(Ast.Expression) {
        const i = @intCast(u32, self.ast.expressions.items.len);
        try self.ast.expressions.append(self.allocator, expr);
        return i;
    }

    fn addGlobalDecl(self: *Parser, decl: Ast.GlobalDecl) std.mem.Allocator.Error!Ast.Index(Ast.GlobalDecl) {
        const i = @intCast(u32, self.ast.globals.items.len);
        try self.ast.globals.append(self.allocator, decl);
        return i;
    }
};

test {
    const t2 = std.time.microTimestamp();
    const str2 =
        \\const t1 = @TypeOf(i32, @TypeOf(1, 2, 3));
    ** 1;
    var p2 = std.zig.Ast.parse(std.heap.c_allocator, str2, .zig) catch return;
    defer p2.deinit(std.heap.c_allocator);

    std.debug.print("\ntook: {d}ms\n", .{
        @intToFloat(f64, std.time.microTimestamp() - t2) / std.time.us_per_ms,
    });
}

test {
    const t = std.time.microTimestamp();
    const str =
        \\type t1 = array<i32, vec3(1, 2, 3)>;
        \\type t1 = array<i32, bitcast<f16>(1)>;
    ** 1;
    var p = try parse(std.heap.c_allocator, str, .{});
    defer p.deinit(std.heap.c_allocator);

    std.debug.print("\ntook: {d}ms\n", .{
        @intToFloat(f64, std.time.microTimestamp() - t) / std.time.us_per_ms,
    });

    const t1 = p.globals.items[0].type_alias;
    const array_i32 = t1.type.array;
    const array_i32_elem_type = p.types.items[array_i32.element_type];
    try std.testing.expectEqual(Ast.PlainType{ .scalar = .i32 }, array_i32_elem_type);

    const vec3_expr = p.expressions.items[array_i32.size.static].construct;
    const vec3_comps = p.expressions.items[vec3_expr.components.multi.start..vec3_expr.components.multi.end];
    try std.testing.expectEqual(
        @as(std.meta.fieldInfo(Ast.ConstructExpr, .type).type, .{ .vector = .{ .partial = .{ .size = .tri } } }),
        vec3_expr.type,
    );
    try std.testing.expectEqual(@as(usize, 3), vec3_comps.len);
    try std.testing.expectEqual(
        Ast.Expression{ .literal = .{ .number = .{ .abstract_int = 2 } } },
        vec3_comps[1],
    );
    try std.testing.expectEqual(
        Ast.Expression{ .literal = .{ .number = .{ .abstract_int = 3 } } },
        vec3_comps[2],
    );
}
