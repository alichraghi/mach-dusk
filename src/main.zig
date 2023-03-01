pub const Token = @import("Token.zig");
pub const Tokenizer = @import("Tokenizer.zig");
pub const Ast = @import("Ast.zig");

const std = @import("std");
test {
    std.testing.refAllDeclsRecursive(Token);
    std.testing.refAllDeclsRecursive(Tokenizer);
    std.testing.refAllDeclsRecursive(Ast);
    std.testing.refAllDeclsRecursive(@import("ErrorList.zig"));
    std.testing.refAllDeclsRecursive(@import("TokenList.zig"));
    std.testing.refAllDeclsRecursive(@import("Parser.zig"));
    std.testing.refAllDeclsRecursive(@import("resolve.zig"));
}
