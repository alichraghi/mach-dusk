pub const Token = @import("Token.zig");
pub const Tokenizer = @import("Tokenizer.zig");

const std = @import("std");
test {
    std.testing.refAllDeclsRecursive(Token);
    std.testing.refAllDeclsRecursive(Tokenizer);
    std.testing.refAllDeclsRecursive(@import("parser.zig"));
}
