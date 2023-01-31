const std = @import("std");
const Token = @import("tokenizer.zig").Token;
const Tokenizer = @import("tokenizer.zig").Tokenizer;

pub const Node = struct {
    tag: Tag,
    main_token: Token,
    lhs: Index,
    rhs: Index,

    pub const Index = u32;
    pub const Tag = enum {
        /// var a: lhs = rhs;
        /// both lhs and rhs may be zero
        /// `var` is main_token
        var_decl,
        /// lhs and rhs is zero
        type,
        /// lhs and rhs is zero
        value,
    };
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    tokens: []const Token,
    tok_i: u32,
    errors: std.ArrayListUnmanaged(Error),
    nodes: std.ArrayListUnmanaged(Node),

    pub const Error = struct {
        token: Token,
        tag: Tag,

        pub const Tag = union(enum) {
            expected_token: Token.Tag,
            expected_type_expr,
        };
    };

    pub fn init(allocator: std.mem.Allocator, source: [:0]const u8) std.mem.Allocator.Error!Parser {
        var tokenizer = Tokenizer.init(source);
        var tokens = std.ArrayList(Token).init(allocator);
        errdefer tokens.deinit();
        while (true) {
            const tok = tokenizer.next();
            try tokens.append(tok);
            if (tok.tag == .eof) break;
        }

        return .{
            .allocator = allocator,
            .source = source,
            .tokens = try tokens.toOwnedSlice(),
            .tok_i = 0,
            .errors = .{},
            .nodes = .{},
        };
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit(self.allocator);
        self.nodes.deinit(self.allocator);
        self.allocator.free(self.tokens);
    }

    pub fn parseRoot(self: *Parser) !void {
        while (true) {
            const token = self.nextToken();
            switch (token.tag) {
                .keyword_var => _ = try self.parseVarDecl(),
                .eof => break,
                else => {},
            }
        }

        if (self.errors.items.len > 0) {
            for (self.errors.items) |err| {
                self.printError(err);
            }
        }
    }

    pub fn parseVarDecl(self: *Parser) !?Node.Index {
        const name = try self.expectToken(.identifier) orelse return null;
        const type_node = if (self.eatToken(.colon) != null) (try self.parseTypeExpr() orelse return null) else 0;
        const value_node = if (self.eatToken(.equal) != null) (try self.parseExpr() orelse return null) else 0;
        _ = name;
        _ = type_node;
        _ = value_node;
        return null;
    }

    pub fn parseTypeExpr(self: *Parser) !?Node.Index {
        const token = self.nextToken();
        switch (token.tag) {
            .keyword_array,
            .keyword_i32,
            .keyword_u32,
            .keyword_f16,
            .keyword_f32,
            .keyword_vec2,
            .keyword_vec3,
            .keyword_vec4,
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
                return try self.addNode(.{
                    .tag = .type,
                    .main_token = token,
                    .lhs = 0,
                    .rhs = 0,
                });
            },
            else => {
                try self.addError(.{
                    .token = token,
                    .tag = .{ .expected_type_expr = {} },
                });
                return null;
            },
        }
    }

    pub fn parseExpr(self: *Parser) !?Node.Index {
        const token = self.nextToken();
        switch (token.tag) {
            .int,
            .int_u,
            .int_i,
            .float,
            .float_f,
            .float_h,
            => {
                return try self.addNode(.{
                    .tag = .literal,
                    .main_token = token,
                    .lhs = 0,
                    .rhs = 0,
                });
            },
            else => {
                try self.addError(.{
                    .token = token,
                    .tag = .{ .expected_type_expr = {} }, // TODO
                });
                return null;
            },
        }
    }

    /// if current token index tag == `tag` returns next token
    /// otherwise returns null and appends `expected_token` error
    pub fn expectToken(self: *Parser, tag: Token.Tag) error{OutOfMemory}!?Token {
        if (self.tokens[self.tok_i].tag == tag) {
            return self.nextToken();
        } else {
            try self.addError(.{
                .token = self.tokens[self.tok_i],
                .tag = .{ .expected_token = tag },
            });
            return null;
        }
    }

    /// if current token index tag == `tag` returns next token
    /// otherwise returns null
    pub fn eatToken(self: *Parser, tag: Token.Tag) ?Token {
        return if (self.tokens[self.tok_i].tag == tag) self.nextToken() else null;
    }

    pub fn nextToken(self: *Parser) Token {
        const current = self.tokens[self.tok_i];
        self.tok_i += 1;
        return current;
    }

    fn addNode(self: *Parser, node: Node) std.mem.Allocator.Error!Node.Index {
        const result = @intCast(Node.Index, self.nodes.items.len);
        try self.nodes.append(self.allocator, node);
        return result;
    }

    fn addError(self: *Parser, err: Error) std.mem.Allocator.Error!void {
        try self.errors.append(self.allocator, err);
    }

    pub fn printError(self: Parser, err: Error) void {
        const stdout_file = std.io.getStdErr().writer();
        var bw = std.io.bufferedWriter(stdout_file);
        const bw_writer = bw.writer();
        const cfg = std.debug.TTY.Config{ .escape_codes = {} };
        const loc_extra = err.token.loc.extraInfo(self.source);

        cfg.setColor(bw_writer, .Bold) catch {};
        bw_writer.print(":{d}:{d} ", .{ loc_extra.line, loc_extra.col }) catch {};

        cfg.setColor(bw_writer, .Red) catch {};
        bw_writer.writeAll("error: ") catch {};

        cfg.setColor(bw_writer, .Reset) catch {};
        cfg.setColor(bw_writer, .Bold) catch {};
        switch (err.tag) {
            .expected_token => |expected_token| {
                bw_writer.print("expected '{s}', found '{s}'", .{
                    expected_token.symbol(),
                    err.token.tag.symbol(),
                }) catch {};
            },
            .expected_type_expr => {
                bw_writer.print("expected type expression, found '{s}'", .{
                    err.token.tag.symbol(),
                }) catch {};
            },
        }
        bw_writer.writeByte('\n') catch {};

        cfg.setColor(bw_writer, .Reset) catch {};
        bw_writer.writeAll(self.source[loc_extra.line_start..loc_extra.line_end]) catch {};
        bw_writer.writeByte('\n') catch {};

        bw_writer.writeByteNTimes(' ', loc_extra.col - 1) catch {};
        cfg.setColor(bw_writer, .Bold) catch {};
        cfg.setColor(bw_writer, .Green) catch {};
        bw_writer.writeByte('^') catch {};
        bw_writer.writeByte('\n') catch {};
        cfg.setColor(bw_writer, .Reset) catch {};

        bw.flush() catch {};
    }
};

test {
    // var d = try std.zig.parse(std.testing.allocator, "var k = 2;");
    // defer d.deinit(std.testing.allocator);
    // for (d.nodes.items(.data)) |data| {
    //     std.debug.print("{}\n", .{data.rhs});
    // }

    const s =
        \\var data = g;
        \\sad
    ;
    var p = try Parser.init(std.testing.allocator, s);
    defer p.deinit();
    try p.parseRoot();
}
