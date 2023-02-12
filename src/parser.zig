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
    errdefer parser.ast.deinit(allocator);

    try parser.ast.globals.ensureTotalCapacityPrecise(allocator, source.len / 300); // 1:300 source to globals
    try parser.ast.expressions.ensureTotalCapacityPrecise(allocator, source.len / 10); // 1:10 source to expressions

    var failed = false;
    while (true) {
        const token = parser.current_token;
        switch (token.tag) {
            .keyword_type => {
                const res = try parser.typeAlias() orelse {
                    failed = true;
                    parser.continueUntilOrEOF(.semicolon);
                    continue;
                };
                _ = try parser.addGlobalDecl(.{ .type_alias = res });
            },
            .keyword_let => {
                const res = try parser.parseVarStatement() orelse {
                    failed = true;
                    parser.continueUntilOrEOF(.semicolon);
                    continue;
                };
                _ = try parser.addGlobalDecl(.{ .variable = res });
            },
            .eof => break,
            else => {
                std.debug.print("Unsupported token ({})\n", .{token.tag});
            },
        }
    }

    if (failed) return error.Parsing;

    return parser.ast;
}

const Parser = struct {
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    tokenizer: Tokenizer,
    current_token: Token,
    error_file: std.fs.File,
    error_lock: bool = false,
    ast: Ast = .{},

    pub fn typeAlias(self: *Parser) !?Ast.TypeAlias {
        // There's no need to check for first token in global decls
        _ = self.next();

        const name = self.expectToken(.ident) orelse return null;
        _ = self.expectToken(.equal) orelse return null;
        const value = try self.expectTypeSpecifier() orelse return null;
        _ = self.expectToken(.semicolon) orelse return null;

        return .{ .name = name.loc.asStr(self.source), .type = value };
    }

    pub fn expectTypeSpecifier(self: *Parser) error{OutOfMemory}!?Ast.Index(Ast.Type) {
        return try self.typeSpecifier() orelse {
            self.addError(
                self.current_token.loc,
                "expected type sepecifier, found '{s}'",
                .{self.current_token.tag.symbol()},
                &.{},
            );
            return null;
        };
    }

    pub fn typeSpecifier(self: *Parser) !?Ast.Index(Ast.Type) {
        if (self.current_token.tag == .ident) {
            _ = self.next();
            return try self.addType(.{ .user = self.current_token.loc.asStr(self.source) });
        }
        return self.typeSpecifierWithoutIdent();
    }

    pub fn typeSpecifierWithoutIdent(self: *Parser) !?Ast.Index(Ast.Type) {
        const token = self.current_token;

        if (self.vectorPrefix()) |vec| {
            _ = self.expectToken(.less_than) orelse return null;
            const elem_type = try self.expectTypeSpecifier() orelse return null;
            _ = self.expectToken(.greater_than) orelse return null;
            return try self.addType(.{ .vector = .{ .prefix = vec, .element = elem_type } });
        }

        if (self.matrixPrefix()) |mat| {
            _ = self.expectToken(.less_than) orelse return null;
            const elem_type = try self.expectTypeSpecifier() orelse return null;
            _ = self.expectToken(.greater_than) orelse return null;
            return try self.addType(.{ .matrix = .{ .prefix = mat, .element = elem_type } });
        }

        switch (token.tag) {
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
                _ = self.expectToken(.less_than) orelse return null;
                const elem_type = try self.expectTypeSpecifier() orelse return null;
                _ = self.expectToken(.greater_than) orelse return null;
                return try self.addType(.{ .atomic = .{ .element = elem_type } });
            },
            .keyword_array => {
                _ = self.expectToken(.less_than) orelse return null;
                const elem_type = try self.expectTypeSpecifier() orelse return null;
                if (self.eatToken(.comma)) |_| {
                    const expr_token = self.current_token;
                    const size = try self.elementCountExpr() orelse {
                        self.addError(
                            expr_token.loc,
                            "expected array size expression, found '{s}'",
                            .{expr_token.tag.symbol()},
                            &.{},
                        );
                        return null;
                    };
                    _ = self.expectToken(.greater_than) orelse return null;
                    return try self.addType(.{ .array = .{
                        .element = elem_type,
                        .size = .{ .static = size },
                    } });
                }
                _ = self.expectToken(.greater_than) orelse return null;
                return try self.addType(.{ .array = .{ .element = elem_type, .size = .dynamic } });
            },
            else => return null,
        }
    }

    pub fn elementCountExpr(self: *Parser) !?Ast.Index(Ast.Expression) {
        const left = try self.unaryExpr() orelse return null;
        _ = left;
        // TODO
        return null;
    }

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

    pub fn primaryExpr(self: *Parser) !?Ast.Index(Ast.Expression) {
        const start_token = self.current_token;

        // TODO: https://gpuweb.github.io/gpuweb/wgsl/#syntax-type_specifier_without_ident
        // 'ptr' '<' address_space ',' type_specifier ( ',' access_mode ) ? '>'
        // texture_and_sampler_types
        if (try self.callable()) |call| {
            const args = try self.expectArgumentExpressionList() orelse return null;
            return try self.addExpr(.{ .call = .{ .callable = call, .args = args } });
        }
        self.regress(start_token);

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
                    .number = parseNumberLiteral(token.loc.asStr(self.source)) catch unreachable,
                } });
            },
            .keyword_bitcast => {
                _ = self.expectToken(.less_than) orelse return null;
                const dest_type = try self.expectTypeSpecifier() orelse return null;
                _ = self.expectToken(.greater_than) orelse return null;
                const args = try self.expectParenExpr() orelse return null;
                return try self.addExpr(.{ .bitcast = .{ .dest = dest_type, .expr = args } });
            },
            .paren_left => return try self.expectParenExpr() orelse return null,
            .ident => {
                if (self.peek().tag == .paren_left) {
                    const args = try self.expectArgumentExpressionList() orelse return null;
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

    pub fn expectParenExpr(self: *Parser) !?Ast.Index(Ast.Expression) {
        _ = self.expectToken(.paren_left) orelse return null;
        const expr = try self.expression() orelse {
            self.addError(
                self.current_token.loc,
                "unable to parse expression '{s}'",
                .{self.current_token.tag.symbol()},
                &.{},
            );
            return null;
        };
        _ = self.expectToken(.paren_right) orelse return null;
        return expr;
    }

    pub fn callable(self: *Parser) error{OutOfMemory}!?Ast.CallExpr.Callable {
        const start_token = self.current_token;

        if (try self.typeSpecifierWithoutIdent()) |ty_i| {
            switch (self.ast.getType(ty_i)) {
                .scalar => |p| return .{ .scalar = p },
                .vector => |p| return .{ .vector = p },
                .matrix => |p| return .{ .matrix = p },
                .array => |p| return .{ .array = p },
                else => return null,
            }
        }
        self.regress(start_token);

        if (self.vectorPrefix()) |vec| return .{ .partial_vector = vec };
        if (self.matrixPrefix()) |mat| return .{ .partial_matrix = mat };
        if (start_token.tag == .keyword_array) {
            _ = self.next();
            return .partial_array;
        }

        return null;
    }

    // TODO
    pub fn expectArgumentExpressionList(self: *Parser) error{OutOfMemory}!?Ast.Span(Ast.Index(Ast.Expression)) {
        var args = std.BoundedArray(Ast.Index(Ast.Expression), max_call_args).init(0) catch unreachable;
        _ = self.expectToken(.paren_left) orelse return null;
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
                return null;
            };
            if (self.current_token.tag != .comma) break;
        }
        _ = self.expectToken(.paren_right) orelse return null;

        if (args.len == 1) {
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

    pub fn unaryExpr(self: *Parser) error{OutOfMemory}!?Ast.Index(Ast.Expression) {
        const start_token = self.current_token;

        const op: Ast.UnaryExpr.Operation = switch (self.current_token.tag) {
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
                .{start_token.tag.symbol()},
                &.{},
            );
            return null;
        };

        return try self.addExpr(.{ .unary = .{ .op = op, .expr = expr } });
    }

    pub fn singularExpr(self: *Parser) !?Ast.Index(Ast.Expression) {
        return self.primaryExpr();
        // TODO: component_or_swizzle_specifier
    }

    pub fn expectMultiplicativeExpr(self: *Parser, left: Ast.Index(Ast.Expression)) !?Ast.Index(Ast.Expression) {
        var left_result = left;
        while (true) {
            const op_token = self.current_token;
            const op: Ast.BinaryExpr.Operation = switch (op_token.tag) {
                .star => .multiply,
                .division => .divide,
                .mod => .modulo,
                else => return left_result,
            };
            _ = self.next();
            const right = try self.unaryExpr() orelse {
                self.addError(
                    self.current_token.loc,
                    "unable to parse right side of '{s}' expression",
                    .{op_token.tag.symbol()},
                    &.{},
                );
                return null;
            };
            left_result = try self.addExpr(.{ .binary = .{ .op = op, .left = left_result, .right = right } });
        }

        unreachable;
    }

    pub fn expectAdditiveExpr(self: *Parser, left: Ast.Index(Ast.Expression)) !?Ast.Index(Ast.Expression) {
        var left_result = left;
        while (true) {
            const op_token = self.current_token;
            const op: Ast.BinaryExpr.Operation = switch (op_token.tag) {
                .plus => .add,
                .minus => .subtract,
                else => return left_result,
            };
            _ = self.next();
            const unary = try self.unaryExpr() orelse {
                self.addError(
                    self.current_token.loc,
                    "unable to parse right side of '{s}' expression",
                    .{op_token.tag.symbol()},
                    &.{},
                );
                return null;
            };
            const right = try self.expectMultiplicativeExpr(unary) orelse return null;
            left_result = try self.addExpr(.{ .binary = .{ .op = op, .left = left_result, .right = right } });
        }

        unreachable;
    }

    pub fn expectMathExpr(self: *Parser, left: Ast.Index(Ast.Expression)) !?Ast.Index(Ast.Expression) {
        const right = try self.expectMultiplicativeExpr(left) orelse return null;
        return self.expectAdditiveExpr(right);
    }

    pub fn shiftExpr(self: *Parser) !?Ast.Index(Ast.Expression) {
        const left = try self.unaryExpr() orelse return null;
        return self.expectShiftExpr(left);
    }

    pub fn expectShiftExpr(self: *Parser, left: Ast.Index(Ast.Expression)) !?Ast.Index(Ast.Expression) {
        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operation = switch (op_token.tag) {
            .shift_left => .shift_left,
            .shift_right => .shift_right,
            else => return self.expectMathExpr(left),
        };
        _ = self.next();

        const right = try self.unaryExpr() orelse {
            self.addError(
                self.current_token.loc,
                "unable to parse right side of '{s}' expression",
                .{op_token.tag.symbol()},
                &.{},
            );
            return null;
        };

        return try self.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
    }

    pub fn relationalExpr(self: *Parser) !?Ast.Index(Ast.Expression) {
        const left = try self.unaryExpr() orelse return null;
        return self.expectRelationalExpr(left);
    }

    pub fn expectRelationalExpr(self: *Parser, left: Ast.Index(Ast.Expression)) !?Ast.Index(Ast.Expression) {
        const left_result = try self.expectShiftExpr(left) orelse return null;
        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operation = switch (op_token.tag) {
            .equal_equal => .equal,
            .not_equal => .not_equal,
            .less_than => .less,
            .less_than_equal => .less_equal,
            .greater_than => .greater,
            .greater_than_equal => .greater_equal,
            else => return left_result,
        };
        _ = self.next();

        const right = try self.shiftExpr() orelse {
            self.addError(
                self.current_token.loc,
                "unable to parse right side of '{s}' expression",
                .{op_token.tag.symbol()},
                &.{},
            );
            return null;
        };
        return try self.addExpr(.{ .binary = .{ .op = op, .left = left_result, .right = right } });
    }

    pub fn bitwiseExpr(self: *Parser, left: Ast.Index(Ast.Expression)) !?Ast.Index(Ast.Expression) {
        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operation = switch (op_token.tag) {
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
                return null;
            };

            left_result = try self.addExpr(.{ .binary = .{ .op = op, .left = left_result, .right = right } });

            if (self.current_token.tag != op_token.tag) return left_result;
        }

        unreachable;
    }

    pub fn expression(self: *Parser) !?Ast.Index(Ast.Expression) {
        const left = try self.unaryExpr() orelse return null;
        if (try self.bitwiseExpr(left)) |bitwise| {
            return bitwise;
        }

        var ret = try self.expectRelationalExpr(left) orelse return null;

        const op_token = self.current_token;
        const op: Ast.BinaryExpr.Operation = switch (op_token.tag) {
            .and_and => .circuit_and,
            .or_or => .circuit_or,
            else => return ret,
        };

        var ret_result = ret;
        while (true) {
            const token = self.current_token;
            if (token.tag != op_token.tag) break;
            _ = self.next();

            const right = try self.relationalExpr() orelse {
                self.addError(
                    self.current_token.loc,
                    "unable to parse right side of '{s}' expression",
                    .{op_token.tag.symbol()},
                    &.{},
                );
                return null;
            };

            ret_result = try self.addExpr(.{ .binary = .{ .op = op, .left = ret, .right = right } });
        }

        return ret_result;
    }

    pub fn parseVarStatement(self: *Parser) !?Ast.VariableStatement {
        const token = self.next();
        switch (token.tag) {
            .keyword_let => {
                const name = self.expectToken(.ident) orelse return null;
                const _type = if (self.eatToken(.colon)) |_| blk: {
                    const type_token = self.current_token;
                    break :blk try self.typeSpecifier() orelse {
                        self.addError(
                            type_token.loc,
                            "expected type sepecifier, but found '{s}'",
                            .{type_token.tag.symbol()},
                            &.{},
                        );
                        return null;
                    };
                } else null;
                _ = self.expectToken(.equal) orelse return null;
                const value = try self.expression() orelse return null;
                _ = self.expectToken(.semicolon) orelse return null;
                return .{
                    .constant = false,
                    .name = name.loc.asStr(self.source),
                    .type = _type,
                    .value = value,
                };
            },

            // TODO

            else => return null,
        }
    }

    pub fn expectToken(self: *Parser, tag: Token.Tag) ?Token {
        const token = self.next();
        if (token.tag == tag) return token;
        self.addError(
            token.loc,
            "expected '{s}', but found '{s}'",
            .{ @tagName(tag), @tagName(token.tag) },
            &.{},
        );
        return null;
    }

    pub fn eatToken(self: *Parser, tag: Token.Tag) ?Token {
        return if (self.current_token.tag == tag) self.next() else null;
    }

    pub fn match(self: *Parser, tag: Token.Tag) ?Token {
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

    pub fn regress(self: *Parser, token: Token) void {
        self.current_token = token;
        self.tokenizer.index = token.loc.end;
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

    pub fn addType(self: *Parser, t: Ast.Type) std.mem.Allocator.Error!Ast.Index(Ast.Type) {
        const i = @intCast(u32, self.ast.types.items.len);
        try self.ast.types.append(self.allocator, t);
        return i;
    }

    pub fn addError(self: Parser, loc: Token.Loc, comptime err_fmt: []const u8, fmt_args: anytype, notes: []const []const u8) void {
        if (self.error_lock) return;

        var bw = std.io.bufferedWriter(self.error_file.writer());
        const b = bw.writer();
        const term = std.debug.TTY.Config{ .escape_codes = {} };
        const loc_extra = loc.extraInfo(self.source);

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
        b.print("{d} ╎ ", .{loc_extra.line}) catch unreachable;
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
            // 'note: '
            term.setColor(b, .Cyan) catch unreachable;
            b.writeAll("note: ") catch unreachable;

            // note
            term.setColor(b, .Reset) catch unreachable;
            term.setColor(b, .Bold) catch unreachable;
            b.writeAll(note) catch unreachable;
            b.writeByte('\n') catch unreachable;
        }

        // clean up and flush
        term.setColor(b, .Reset) catch unreachable;
        bw.flush() catch unreachable;
    }

    // TODO
    pub fn parseNumberLiteral(str: []const u8) !Ast.Literal.Number {
        return .{ .abstract_int = try std.fmt.parseInt(
            i64,
            str,
            10,
        ) };
    }
};

const expectEqual = std.testing.expectEqual;
test {
    std.testing.refAllDeclsRecursive(Parser);
    const source =
        // \\let ali = vec<f32>(1);
        \\let ali = 1 + 5 + 2 * 3 > 6 >> 7;
    ;

    var ast = try parse(std.testing.allocator, source, null);
    defer ast.deinit(std.testing.allocator);

    const greater = ast.getExpr(ast.getGlobal(0).variable.value).binary;
    const add = ast.getExpr(greater.left).binary;
    const add_left = ast.getExpr(add.left).binary;
    const add_left_left = ast.getExpr(add_left.left).literal;
    const add_left_right = ast.getExpr(add_left.right).literal;
    const add_right = ast.getExpr(add.right).binary;
    // const shift = ast.getExpr(greater.right).binary;
    std.debug.print("\n\n{}\n{}\n{}\n{}\n{}\n", .{
        add,
        add_left,
        add_left_left,
        add_left_right,
        add_right,
    });
    // std.debug.print(
    //     \\
    //     \\
    //     \\Greater: {}
    //     \\ --> Add:   {}
    //     \\   --> Number: {}
    //     \\   --> Add:    {}
    //     \\     --> Number: {}
    //     \\     --> Mul:    {}
    //     \\ --> Shift: {}
    //     \\
    //     \\
    // , .{ greater, add, add_left, add_right, shift });

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
