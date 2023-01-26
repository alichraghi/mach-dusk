pub const Token = @import("tokenizer.zig").Token;
pub const Tokenizer = @import("tokenizer.zig").Tokenizer;

const std = @import("std");
test {
    std.testing.refAllDeclsRecursive(Token);
    std.testing.refAllDeclsRecursive(Tokenizer);
}
