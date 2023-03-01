const Ast = @import("Ast.zig");
const IR = @This();

pub fn generate(ast: Ast) !IR {
    _ = ast;
    return .{};
}

pub const TranslationUnit = struct {
    decls: GlobalDecl,
};

pub const GlobalDecl = union(enum) {
    // pub const Variable = struct {
    //     type: Type,
    // };
};
