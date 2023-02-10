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

    try parser.ast.globals.ensureTotalCapacityPrecise(allocator, source.len / 300); // 1:300 source to globals
    try parser.ast.expressions.ensureTotalCapacityPrecise(allocator, source.len / 10); // 1:10 source to expressions

    while (true) {
        const token = parser.current_token;
        switch (token.tag) {
            .keyword_type => {
                const res = parser.parseTypeAlias() catch |err| switch (err) {
                    error.Parsing => {
                        while (parser.nextToken().tag != .semicolon) {} // TODO: wrong
                        continue;
                    },
                    else => return err,
                };
                _ = try parser.addGlobalDecl(.{ .type_alias = res });
            },
            .keyword_let => {
                const res = parser.parseVarStatement() catch |err| switch (err) {
                    error.Parsing => {
                        continue;
                    },
                    else => return err,
                };
                std.debug.print("\n{} \n {} \n {}\n", .{
                    parser.ast.getExpr(res.value),
                    parser.ast.getExpr(parser.ast.getExpr(res.value).binary.left),
                    parser.ast.getExpr(parser.ast.getExpr(res.value).binary.right),
                });
            },
            .eof => break,
            else => {
                std.debug.print("Unsupported token ({})\n", .{token.tag});
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
            expected_unary_expr,
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
            expected_fixed_array_type,
            expected_call_expr,
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
                .expected_unary_expr => {
                    try bw_writer.print("expected unary expression, but found '{s}'", .{
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
                    try bw_writer.print("expected at least {} arguments", .{min});
                },
                .exceeded_max_args => |max| {
                    try bw_writer.print("exceeded maximum call arguments ({})", .{max});
                },
                .invalid_bitcast_dest_type => {
                    try bw_writer.print("invalid bitcast destination type. must be eathir ('i32', 'u32' or 'f32') but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
                .expected_fixed_array_type => {
                    try bw_writer.print("expected a fixed size array", .{});
                },
                .expected_call_expr => {
                    try bw_writer.print("expected a call expression, but found '{s}'", .{
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

    /// TypeAlias <- KEYWORD_type IDENTIFIER EQUAL PlainType
    pub fn parseTypeAlias(self: *Parser) !Ast.TypeAlias {
        // There's no need to check for first token in global decls
        _ = self.nextToken();

        const name = try self.expectToken(.ident);
        _ = try self.expectToken(.equal);
        const value = try self.parsePlainType();
        _ = try self.expectToken(.semicolon);

        return .{ .name = name.loc.asStr(self.source), .type = value };
    }

    /// PlainType <- ScalarType
    ///            | VectorType.Full
    ///            | MatrixType.Full
    ///            | AtomicType
    ///            | ArrayType
    ///            | IDENTIFIER
    pub fn parsePlainType(self: *Parser) !Ast.PlainType {
        const token = self.current_token;
        switch (token.tag.group()) {
            .scalar => return .{ .scalar = try self.parseScalarType() },
            .sampler => return .{ .sampler = try self.parseSamplerType() },
            .vector => return .{ .vector = try self.parseVectorType(true) },
            .matrix => return .{ .matrix = try self.parseMatrixType(true) },
            .atomic => return .{ .atomic = try self.parseAtomicType() },
            .array => return .{ .array = try self.parseArrayType(false) },
            .ident => {
                _ = self.nextToken();
                return .{ .user = token.loc.asStr(self.source) };
            },
            else => {
                try self.addError(.{ .token = token, .tag = .expected_type });
                return error.Parsing;
            },
        }
    }

    /// ScalarType <- KEYWORD_i32
    ///             | KEYWORD_u32
    ///             | KEYWORD_f32
    ///             | KEYWORD_f16
    ///             | KEYWORD_bool
    pub fn parseScalarType(self: *Parser) !Ast.ScalarType {
        const token = self.nextToken();
        return switch (token.tag) {
            .keyword_i32 => .i32,
            .keyword_u32 => .u32,
            .keyword_f32 => .f32,
            .keyword_f16 => .f16,
            .keyword_bool => .bool,
            else => {
                try self.addError(.{ .token = token, .tag = .expected_scalar_type });
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
                try self.addError(.{ .token = token, .tag = .expected_sampler_type });
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
                try self.addError(.{ .token = token, .tag = .expected_vector_type });
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
    /// MatrixType.Partial <- KEYWORD_mat2x2
    ///                     | KEYWORD_mat2x3
    ///                     | KEYWORD_mat2x4
    ///                     | KEYWORD_mat3x2
    ///                     | KEYWORD_mat3x3
    ///                     | KEYWORD_mat3x4
    ///                     | KEYWORD_mat4x2
    ///                     | KEYWORD_mat4x3
    ///                     | KEYWORD_mat4x4
    pub fn parseMatrixType(self: *Parser, strict: bool) !Ast.MatrixType {
        const token = self.nextToken();
        if (token.tag.group() != .matrix) {
            try self.addError(.{ .token = token, .tag = .expected_matrix_type });
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
                try self.addError(.{ .token = elem_type_token, .tag = .expected_float_type });
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
            try self.addError(.{ .token = token, .tag = .expected_atomic_type });
            return error.Parsing;
        }

        _ = try self.expectToken(.less_than);
        const elem_type = try self.parseScalarType();
        _ = try self.expectToken(.greater_than);

        return .{ .element_type = elem_type };
    }

    /// ArrayType <- KEYWORD_array LESS_THAN PlainType GREATER_THAN
    ///
    /// NOTE: ArrayType and StructType elements must have a creation-fixed footprint
    pub fn parseArrayType(self: *Parser, fixed_only: bool) error{ Parsing, OutOfMemory }!Ast.ArrayType {
        const token = self.nextToken();
        if (token.tag != .keyword_array) {
            try self.addError(.{ .token = token, .tag = .expected_array_type });
            return error.Parsing;
        }

        _ = try self.expectToken(.less_than);
        const elem_type = @intCast(u32, self.ast.types.items.len);
        try self.ast.types.append(self.allocator, try self.parsePlainType());

        if (self.eatToken(.comma)) |_| {
            const size = try self.parseUnaryExpr();
            _ = try self.expectToken(.greater_than);
            return .{ .element_type = elem_type, .size = .{ .static = size } };
        } else {
            if (fixed_only and self.current_token.tag == .greater_than) {
                try self.addError(.{ .token = token, .tag = .expected_fixed_array_type });
                return error.Parsing;
            }
        }

        _ = try self.expectToken(.greater_than);

        return .{ .element_type = elem_type, .size = .dynamic };
    }

    /// Expression <- LiteralExpr | ConstructExpr
    pub fn parseExpr(self: *Parser) !Ast.Index(Ast.Expression) {
        const left = try self.parseRelationalExpr(null);
        const pre_bitwise_token = self.current_token;
        return self.parseBitwiseExpr(left) catch |err| switch (err) {
            error.Parsing => {
                self.regressToken(pre_bitwise_token);
                return self.parseShortCircuitExpr(left);
            },
            else => return err,
        };
    }

    /// LiteralExpr <- Number | KEYWORD_true | KEYWORD_false
    pub fn parsePrimaryExpr(self: *Parser) !Ast.Index(Ast.Expression) {
        const token = self.nextToken();
        switch (token.tag) {
            // .paren_left => .{}, TODO
            .keyword_true => return self.addExpr(.{ .literal = .{ .bool = true } }),
            .keyword_false => return self.addExpr(.{ .literal = .{ .bool = false } }),
            .number => return self.addExpr(.{ .literal = .{
                .number = parseNumberLiteral(token.loc.asStr(self.source)) catch unreachable,
            } }),
            .keyword_bitcast => {
                _ = try self.expectToken(.less_than);
                const dest_type = try self.parseScalarType();
                _ = try self.expectToken(.greater_than);
                const args = try self.parseCallArguments(1, 1);
                return self.addExpr(.{ .bitcast = .{ .dest = dest_type, .expr = args.one } });
            },
            .ident => {
                if (self.current_token.tag == .paren_left) {
                    return self.parseCallExpr();
                } else {
                    return self.addExpr(.{ .ident = token.loc.asStr(self.source) });
                }
            },
            else => {
                if (self.current_token.tag == .paren_left) {
                    return self.parseCallExpr();
                }

                try self.addError(.{ .token = token, .tag = .expected_literal_expr });
                return error.Parsing;
            },
        }
    }

    // TODO: https://gpuweb.github.io/gpuweb/wgsl/#syntax-type_specifier_without_ident
    // 'ptr' '<' address_space ',' type_specifier ( ',' access_mode ) ? '>'
    // texture_and_sampler_types
    pub fn parseCallExpr(self: *Parser) !Ast.Index(Ast.Expression) {
        const token = self.current_token;
        switch (token.tag.group()) {
            .ident => {
                const func = token.loc.asStr(self.source);
                _ = self.nextToken();
                return self.addExpr(.{
                    .call = .{
                        .func = func,
                        .args = try self.parseCallArguments(0, max_call_args),
                    },
                });
            },
            .scalar => {
                const scalar_type = try self.parseScalarType();
                const args = try self.parseCallArguments(0, max_call_args);
                return self.addExpr(.{ .construct = .{
                    .type = .{
                        .scalar = scalar_type,
                    },
                    .components = args,
                } });
            },
            .vector => {
                const vector_type = try self.parseVectorType(false);
                const args = try self.parseCallArguments(0, @enumToInt(vector_type.size()));
                return self.addExpr(.{ .construct = .{ .type = .{ .vector = vector_type }, .components = args } });
            },
            .matrix => {
                const matrix_type = try self.parseMatrixType(false);
                const args = try self.parseCallArguments(0, matrix_type.len());
                return self.addExpr(.{ .construct = .{ .type = .{ .matrix = matrix_type }, .components = args } });
            },
            .array => {
                if (self.peekToken().tag == .less_than) {
                    const array_type = try self.parseArrayType(true);
                    const args = try self.parseCallArguments(0, max_call_args);
                    return self.addExpr(.{ .construct = .{
                        .type = .{ .full_array = array_type },
                        .components = args,
                    } });
                }

                _ = self.nextToken();
                const args = try self.parseCallArguments(0, max_call_args);

                return self.addExpr(.{ .construct = .{
                    .type = .partial_array,
                    .components = args,
                } });
            },
            else => {
                try self.addError(.{ .token = token, .tag = .expected_call_expr });
                return error.Parsing;
            },
        }
    }

    // TODO
    pub fn parseNumberLiteral(str: []const u8) !Ast.Literal.Number {
        return .{ .abstract_int = try std.fmt.parseInt(
            i64,
            str,
            10,
        ) };
    }

    /// ConstructExpr <- VectorType
    ///                | MatrixType
    ///                | (KEYWORD_array | ArrayType)
    ///                  LEFT_PAREN ( Expression COMMA? )* RIGHT_PAREN
    pub fn parseVarStatement(self: *Parser) !Ast.VariableStatement {
        const token = self.nextToken();
        switch (token.tag) {
            .keyword_let => {
                const name = try self.expectToken(.ident);
                const _type = if (self.eatToken(.colon)) |_|
                    try self.parsePlainType() // TODO: plain type??!
                else
                    null;
                _ = try self.expectToken(.equal);
                const value = try self.parseExpr();
                _ = try self.expectToken(.semicolon);
                return .{
                    .constant = false,
                    .name = name.loc.asStr(self.source),
                    .type = _type,
                    .value = value,
                };
            },
            else => unreachable, // TODO
        }

        try self.addError(.{
            .token = token,
            .tag = .expected_construct_expr,
        });
        return error.Parsing;
    }

    /// UnaryExpr <- BANG | TILDE | MINUS | STAR | AND UnaryExpr
    pub fn parseUnaryExpr(self: *Parser) !Ast.Index(Ast.Expression) {
        const token = self.current_token;
        const op: Ast.UnaryExpr.Operation = switch (token.tag) {
            .bang, .tilde => .not,
            .minus => .negate,
            .star => .deref,
            .@"and" => .addr_of,
            else => {
                return self.parseSingularExpr();
            },
        };

        _ = self.nextToken();
        return self.addExpr(.{ .unary = .{
            .op = op,
            .expr = try self.parseUnaryExpr(),
        } });
    }

    /// UnaryExpr <- BANG | TILDE | MINUS | STAR | AND UnaryExpr
    pub fn parseSingularExpr(self: *Parser) !Ast.Index(Ast.Expression) {
        return self.parsePrimaryExpr();
        // TODO: component_or_swizzle_specifier
    }

    pub fn parseBitwiseExpr(self: *Parser, left: Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        const left_expr_op = switch (self.ast.getExpr(left)) {
            .binary => |s| s.op,
            else => return error.Parsing,
        };
        switch (left_expr_op) {
            .binary_and => _ = try self.expectToken(.@"and"),
            .binary_or => _ = try self.expectToken(.@"or"),
            .binary_xor => _ = try self.expectToken(.xor),
            else => return error.Parsing,
        }
        const right = try self.parseUnaryExpr();
        return self.addExpr(.{ .binary = .{ .op = left_expr_op, .left = left, .right = right } });
    }

    pub fn parseBinaryExpr(self: *Parser, left_maybe: ?Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        const left = left_maybe orelse try self.parseUnaryExpr();
        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operation = switch (op_token.tag) {
            .@"and" => .binary_and,
            .@"or" => .binary_or,
            .xor => .binary_xor,
            else => return left,
        };
        _ = self.nextToken();
        const right = try self.parseBinaryExpr(null);
        return self.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
    }

    pub fn parseShortCircuitExpr(self: *Parser, left_maybe: ?Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        const left = left_maybe orelse try self.parseRelationalExpr(null);
        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operation = switch (op_token.tag) {
            .and_and => .circuit_and,
            .or_or => .circuit_or,
            else => return left,
        };
        _ = self.nextToken();
        const right = try self.parseShortCircuitExpr(null);
        return self.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
    }

    pub fn parseRelationalExpr(self: *Parser, left_maybe: ?Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        const left = left_maybe orelse try self.parseShiftExpr(null);
        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operation = switch (op_token.tag) {
            .equal_equal => .equal,
            .not_equal => .not_equal,
            .less_than => .less,
            .less_than_equal => .less_equal,
            .greater_than => .greater,
            .greater_than_equal => .greater_equal,
            else => return self.parseShiftExpr(left),
        };
        _ = self.nextToken();
        const right = try self.parseShiftExpr(null);
        return self.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
    }

    pub fn parseShiftExpr(self: *Parser, left_maybe: ?Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        const left = left_maybe orelse try self.parseUnaryExpr();
        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operation = switch (op_token.tag) {
            .shift_left => .shift_left,
            .shift_right => .shift_right,
            else => return self.parseAdditiveExpr(left),
        };
        _ = self.nextToken();
        const right = try self.parseUnaryExpr();
        return self.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
    }

    pub fn parseAdditiveExpr(self: *Parser, left_maybe: ?Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        const left = left_maybe orelse try self.parseMultiplicativeExpr(null);
        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operation = switch (op_token.tag) {
            .plus => .add,
            .minus => .subtract,
            else => return left,
        };
        _ = self.nextToken();
        const right = try self.parseAdditiveExpr(null);
        return self.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
    }

    pub fn parseMultiplicativeExpr(self: *Parser, left_maybe: ?Ast.Index(Ast.Expression)) !Ast.Index(Ast.Expression) {
        const left = left_maybe orelse try self.parseUnaryExpr();
        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operation = switch (op_token.tag) {
            .star => .multiply,
            .division => .divide,
            .mod => .modulo,
            else => return left,
        };
        _ = self.nextToken();
        const right = try self.parseMultiplicativeExpr(null);
        return self.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
    }

    /// CallArguments <- LEFT_PAREN (Expr COMMA?)* RIGHT_PAREN
    pub fn parseCallArguments(self: *Parser, min: u8, max: u8) error{ Parsing, OutOfMemory }!Ast.Span(Ast.Index(Ast.Expression)) {
        std.debug.assert(max <= max_call_args);

        var args = std.BoundedArray(Ast.Index(Ast.Expression), max_call_args).init(0) catch unreachable;
        const l_paren_token = try self.expectToken(.paren_left);

        if (self.current_token.tag == .paren_right) {
            _ = self.nextToken();
        } else while (true) {
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
                .token = l_paren_token,
                .tag = .{ .expected_at_least_x_args = min },
            });
            return error.Parsing;
        }

        if (args.len == 0) {
            return .{ .zero = {} };
        } else if (args.len == 1) {
            try self.ast.extra.append(self.allocator, args.get(0));
            return .{ .one = @intCast(u32, self.ast.extra.items.len - 1) };
        } else {
            try self.ast.extra.appendSlice(self.allocator, args.slice());
            return .{ .multi = .{
                .start = @intCast(u32, self.ast.extra.items.len - args.len),
                .end = @intCast(u32, self.ast.extra.items.len),
            } };
        }
    }

    pub fn expectToken(self: *Parser, tag: Token.Tag) !Token {
        const token = self.nextToken();
        if (token.tag == tag) {
            return token;
        } else {
            try self.addError(.{
                .token = token,
                .tag = .{ .expected_token = tag },
            });
            return error.Parsing;
        }
    }

    pub fn eatToken(self: *Parser, tag: Token.Tag) ?Token {
        return if (self.current_token.tag == tag) self.nextToken() else null;
    }

    pub fn peekToken(self: *Parser) Token {
        return self.tokenizer.peek();
    }

    pub fn nextToken(self: *Parser) Token {
        const current = self.current_token;
        self.current_token = self.tokenizer.next();
        return current;
    }

    pub fn regressToken(self: *Parser, token: Token) void {
        self.current_token = token;
        self.tokenizer.index = token.loc.end;
    }

    pub fn addError(self: *Parser, err: Error) std.mem.Allocator.Error!void {
        try self.errors.append(self.allocator, err);
    }

    pub fn addExpr(self: *Parser, expr: Ast.Expression) std.mem.Allocator.Error!Ast.Index(Ast.Expression) {
        const i = @intCast(u32, self.ast.expressions.items.len);
        try self.ast.expressions.append(self.allocator, expr);
        return i;
    }

    pub fn addGlobalDecl(self: *Parser, decl: Ast.GlobalDecl) std.mem.Allocator.Error!Ast.Index(Ast.GlobalDecl) {
        const i = @intCast(u32, self.ast.globals.items.len);
        try self.ast.globals.append(self.allocator, decl);
        return i;
    }
};

// test {
//     const t2 = std.time.microTimestamp();
//     const str2 =
//         \\const t1 = @TypeOf(i32, @TypeOf(1, 2, 3));
//     ** 1;
//     var p2 = std.zig.Ast.parse(std.heap.c_allocator, str2, .zig) catch return;
//     defer p2.deinit(std.heap.c_allocator);

//     std.debug.print("\ntook: {d}ms\n", .{
//         @intToFloat(f64, std.time.microTimestamp() - t2) / std.time.us_per_ms,
//     });
// }

const expectEqual = std.testing.expectEqual;
test {
    std.testing.refAllDeclsRecursive(Parser);
    const source =
        \\let ali = 1 + 2 * 3 / 4 - 1 > 6 >> 7;
    ;

    var ast = try parse(std.testing.allocator, source, .{});
    defer ast.deinit(std.testing.allocator);

    // const t1_type = ast.getGlobal(0).type_alias.type.array;
    // const t1_elem_type = ast.getPlainType(t1_type.element_type);
    // const t1_size_expr = ast.getExpr(t1_type.size.static).construct;
    // const t1_size_expr_comps = ast.getExprRange(t1_size_expr.components.multi);

    // try expectEqual(Ast.ScalarType.i32, t1_elem_type.scalar);
    // try expectEqual(Ast.VectorType.Size.tri, t1_size_expr.type.vector.partial.size);
    // try expectEqual(@as(i64, 1), t1_size_expr_comps[0].literal.number.abstract_int);
    // try expectEqual(@as(i64, 2), t1_size_expr_comps[1].literal.number.abstract_int);
    // try expectEqual(@as(i64, 3), t1_size_expr_comps[2].literal.number.abstract_int);

    // const t1_dest_type = ast.getGlobal(1).type_alias.type.array;
    // const t1_expr = ast.getPlainType(t1_type.element_type);
}
