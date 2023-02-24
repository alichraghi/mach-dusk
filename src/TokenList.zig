const std = @import("std");
const Token = @import("Token.zig");
const TokenList = @This();

list: []const Token,
index: Index = 0,

pub const Index = u32;

pub fn get(self: TokenList, i: Index) Token {
    return self.list[std.math.min(i, self.list.len)];
}

pub fn peek(self: TokenList, offset: Index) Token {
    return self.get(self.index + offset);
}

pub fn advance(self: *TokenList) Index {
    const prev = self.index;
    self.index = std.math.min(prev +| 1, self.list.len);
    return prev;
}

pub fn advanceUntil(self: *TokenList, until: Token.Tag) void {
    while (true) {
        const tag = self.peek(0).tag;
        if (tag == until or tag == .eof) break;
        _ = self.advance();
    }
}
