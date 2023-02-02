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

    while (true) {
        const tok = tokenizer.next();
        try tokens.append(tok);
        if (tok.tag == .eof) break;
    }

    var parser = Parser{
        .allocator = allocator,
        .source = source,
        .tokens = try tokens.toOwnedSlice(),
        .tok_i = 0,
        .errors = .{},
        .ast = .{ .globals = .{}, .array_bases = .{} },
    };
    defer allocator.free(parser.tokens);
    defer parser.errors.deinit(allocator);

    while (true) {
        const token = parser.nextToken();
        switch (token.tag) {
            .keyword_type => {
                const res = parser.parseTypeAlias() catch |err| switch (err) {
                    error.Parsing => continue,
                    else => return err,
                };
                try parser.ast.globals.append(allocator, .{ .kind = .{ .type_alias = res } });
            },
            .eof => break,
            else => {},
        }
    }

    if (parser.errors.items.len > 0) {
        defer parser.ast.deinit(allocator);
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

    pub fn parseTypeAlias(self: *Parser) !Ast.TypeAlias {
        const name = try self.expectToken(.identifier);
        _ = try self.expectToken(.equal);
        const value = try self.parseTypeDecl();
        _ = try self.expectToken(.semicolon);
        return .{ .name = name.loc.asStr(self.source), .type = value };
    }

    pub fn parseTypeDecl(self: *Parser) !Ast.Type {
        const token = self.nextToken();
        switch (token.tag) {
            .keyword_i32,
            .keyword_u32,
            .keyword_f16,
            .keyword_f32,
            .keyword_bool,
            => {
                return .{ .scalar = tokenToScalarType(token.tag) };
            },
            .keyword_array => {
                _ = try self.expectToken(.less_than);

                var base = try self.ast.array_bases.addOne(self.allocator);
                base.* = try self.parseTypeDecl();

                // dynamic size
                _ = self.eatToken(.comma) orelse {
                    _ = try self.expectToken(.greater_than);
                    return .{ .array = .{ .base = base, .size = .dynamic } };
                };

                // constant size
                const size = try self.parseNumber();
                _ = try self.expectToken(.greater_than);
                return .{ .array = .{ .base = base, .size = .{ .constant = size } } };
            },
            .keyword_vec2, .keyword_vec3, .keyword_vec4 => {
                _ = try self.expectToken(.less_than);
                const base = try self.parseVectorBase();
                _ = try self.expectToken(.greater_than);
                return .{ .vector = .{ .base = base, .size = tokenToVectorSize(token.tag) } };
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
                const base = try self.parseMatrixBase();
                _ = try self.expectToken(.greater_than);
                return .{ .matrix = .{
                    .base = base,
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

    // TODO
    pub fn parseNumber(self: *Parser) !Ast.Number {
        const num = try self.expectToken(.number);
        return .{
            .abstract_int = try std.fmt.parseInt(
                i64,
                num.loc.asStr(self.source),
                10,
            ),
        };
    }

    pub fn parseVectorBase(self: *Parser) !Ast.ScalarType {
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

    pub fn parseMatrixBase(self: *Parser) !Ast.ScalarType {
        const token = self.nextToken();
        switch (token.tag) {
            .keyword_f16,
            .keyword_f32,
            => return tokenToScalarType(token.tag),
            .keyword_bool, .keyword_i32, .keyword_u32 => {
                try self.addError(.{
                    .token = token,
                    .tag = .{ .matrix_base_must_be_float = {} },
                });
                return error.Parsing;
            },
            else => {
                try self.addError(.{
                    .token = token,
                    .tag = .{ .expected_float_type = {} },
                });
                return error.Parsing;
            },
        }
    }

    pub fn expectToken(self: *Parser, tag: Token.Tag) !Token {
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

    pub fn eatToken(self: *Parser, tag: Token.Tag) ?Token {
        return if (self.tokens[self.tok_i].tag == tag) self.nextToken() else null;
    }

    pub fn nextToken(self: *Parser) Token {
        const current = self.tokens[self.tok_i];
        self.tok_i += 1;
        return current;
    }

    pub fn tokenToScalarType(tag: Token.Tag) Ast.ScalarType {
        return switch (tag) {
            .keyword_i32 => .{ .kind = .int, .width = 4 },
            .keyword_u32 => .{ .kind = .uint, .width = 4 },
            .keyword_f32 => .{ .kind = .float, .width = 4 },
            .keyword_f16 => .{ .kind = .float, .width = 2 },
            .keyword_bool => .{ .kind = .bool, .width = 1 },
            else => unreachable,
        };
    }

    pub fn tokenToVectorSize(tag: Token.Tag) Ast.VectorType.Size {
        return switch (tag) {
            .keyword_vec2 => .bi,
            .keyword_vec3 => .tri,
            .keyword_vec4 => .quad,
            else => unreachable,
        };
    }

    pub fn tokenToMatrixColumns(tag: Token.Tag) Ast.VectorType.Size {
        return switch (tag) {
            .keyword_mat2x2, .keyword_mat2x3, .keyword_mat2x4 => .bi,
            .keyword_mat3x2, .keyword_mat3x3, .keyword_mat3x4 => .tri,
            .keyword_mat4x2, .keyword_mat4x3, .keyword_mat4x4 => .quad,
            else => unreachable,
        };
    }

    pub fn tokenToMatrixRows(tag: Token.Tag) Ast.VectorType.Size {
        return switch (tag) {
            .keyword_mat2x2, .keyword_mat3x2, .keyword_mat4x2 => .bi,
            .keyword_mat2x3, .keyword_mat3x3, .keyword_mat4x3 => .tri,
            .keyword_mat2x4, .keyword_mat3x4, .keyword_mat4x4 => .quad,
            else => unreachable,
        };
    }

    pub const Error = struct {
        tag: Tag,
        token: Token,
        notes: []const []const u8 = &.{},

        pub const Tag = union(enum) {
            expected_token: Token.Tag,
            expected_type_decl,
            expected_scalar_type,
            expected_float_type,
            matrix_base_must_be_float,
        };
    };

    pub fn emitErrors(self: Parser) !void {
        const stdout_file = std.io.getStdErr().writer();
        var bw = std.io.bufferedWriter(stdout_file);
        const bw_writer = bw.writer();
        const cfg = std.debug.TTY.Config{ .escape_codes = {} };

        for (self.errors.items) |err| {
            const loc_extra = err.token.loc.extraInfo(self.source);

            // print file:line:column
            try cfg.setColor(bw_writer, .Bold);
            try bw_writer.print(":{d}:{d} ", .{ loc_extra.line, loc_extra.col });

            try cfg.setColor(bw_writer, .Red);
            try bw_writer.writeAll("error: ");

            // print error message
            try cfg.setColor(bw_writer, .Reset);
            try cfg.setColor(bw_writer, .Bold);
            switch (err.tag) {
                .expected_token => |expected_token| {
                    try bw_writer.print("expected '{s}', but found '{s}'", .{
                        expected_token.symbol(),
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
                .matrix_base_must_be_float => {
                    try bw_writer.print("matrix base must be a floating-point type ('f32' or 'f16'), but found '{s}'", .{
                        err.token.tag.symbol(),
                    });
                },
            }
            try bw_writer.writeByte('\n');

            // print error line
            try cfg.setColor(bw_writer, .Reset);
            try bw_writer.writeAll(self.source[loc_extra.line_start..loc_extra.line_end]);
            try bw_writer.writeByte('\n');

            // print error location pointer ('^')
            try bw_writer.writeByteNTimes(' ', loc_extra.col - 1);
            try cfg.setColor(bw_writer, .Bold);
            try cfg.setColor(bw_writer, .Green);
            try bw_writer.writeByte('^');
            try bw_writer.writeByte('\n');
            try cfg.setColor(bw_writer, .Reset);
        }

        try bw.flush();
    }

    fn addError(self: *Parser, err: Error) std.mem.Allocator.Error!void {
        try self.errors.append(self.allocator, err);
    }
};

test {
    const s =
        \\type t1 = mat2x3<gg>;
        \\var data = g;
        \\sad
    ;
    var p = parse(std.testing.allocator, s, .{}) catch return;
    defer p.deinit(std.testing.allocator);
    std.debug.print("\n\n{s}\n{}\n\n", .{
        p.globals.items[0].kind.type_alias.name,
        p.globals.items[0].kind.type_alias.type,
    });
}
