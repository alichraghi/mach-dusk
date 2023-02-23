const std = @import("std");
const Token = @import("Token.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");

const Ast = @This();

nodes: std.MultiArrayList(Node) = .{},
extra_data: std.ArrayListUnmanaged(Node.Index) = .{},
scratch: std.ArrayListUnmanaged(Node.Index) = .{},
tokens: []const Token,

/// parses a TranslationUnit(WGSL Program)
pub fn parse(
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    error_file: ?std.fs.File,
) !Ast {
    var tokenizer = Tokenizer.init(source);
    const estimated_tokens = source.len / 8;
    var tokens = try std.ArrayList(Token).initCapacity(allocator, estimated_tokens);
    while (true) {
        const t = tokenizer.next();
        try tokens.append(t);
        if (t.tag == .eof) break;
    }

    var parser = Parser{
        .allocator = allocator,
        .source = source,
        .ast = .{ .tokens = try tokens.toOwnedSlice() },
        .tok_i = 0,
        .error_file = error_file orelse std.io.getStdErr(),
    };
    errdefer parser.ast.deinit(allocator);

    const estimated_nodes = (parser.ast.tokens.len + 2) / 2;
    try parser.ast.nodes.ensureTotalCapacity(allocator, estimated_nodes);

    _ = parser.addNode(.{
        .tag = .translation_unit,
        .main_token = 0,
    }) catch unreachable;

    const scratch_top = parser.ast.scratch.items.len;
    defer parser.ast.scratch.shrinkRetainingCapacity(scratch_top);

    while (parser.currentToken().tag != .eof) {
        const decl = parser.expectGlobalDecl() catch |err| {
            if (err == error.Parsing) {
                parser.continueUntilOrEOF(.semicolon);
                continue;
            } else return err;
        };
        try parser.ast.scratch.append(allocator, decl);
    }

    const list = parser.ast.scratch.items[scratch_top..];
    try parser.ast.extra_data.appendSlice(allocator, list);
    parser.ast.nodes.items(.lhs)[0] = @intCast(Ast.Node.Index, parser.ast.extra_data.items.len - list.len);
    parser.ast.nodes.items(.rhs)[0] = @intCast(Ast.Node.Index, parser.ast.extra_data.items.len);

    return parser.ast;
}

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    self.nodes.deinit(allocator);
    self.extra_data.deinit(allocator);
    self.scratch.deinit(allocator);
    allocator.free(self.tokens);
}

pub const TokenIndex = u32;
pub const null_node: Node.Index = 0;

pub const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
    lhs: Index = null_node,
    rhs: Index = null_node,

    pub const Index = u32;

    pub const Tag = enum {
        /// a helper node pointing at extra_data[lhs..rhs].
        span,

        /// root node pointing at extra_data[lhs..rhs].
        translation_unit,

        // ********* Global declarations *********
        /// main_token is 'var'.
        /// lhs is a VarDecl.
        /// rhs is initializer [Optional]
        global_variable,

        // ********* Types *********
        /// main_token is ScalarType.
        scalar_type,
        /// main_token is SamplerType.
        sampler_type,
        /// vec2<lhs>
        /// main_token is VectorPrefix.
        /// lhs is element type.
        vector_type,
        /// mat2x2<lhs>
        /// main_token is MatrixPrefix
        /// lhs is element type.
        matrix_type,
        /// atomic<lhs>
        /// main_token is 'atomic'.
        /// lhs is element type.
        atomic_type,
        /// array<lhs, rhs>
        /// main_token is 'array'.
        /// lhs is element type.
        /// rhs is array size [Optional]
        array_type,
        /// ptr<rhs.addr_space, lhs, rhs.access_mode>
        /// main_token is 'ptr'.
        /// lhs is element type.
        /// rhs is PtrType.
        /// rhs.access_mode may be null.
        ptr_type,
        /// main_token is identifier
        user_type,

        // ********* Attributes *********
        // main_token is '@'
        /// @const
        attr,
        /// @align(lhs)
        /// lhs is Expr
        attr_one_arg,
        /// @builtin(lhs)
        /// lhs is TokenIndex
        attr_builtin,
        /// @workgroup_size(lhs, rhs.y, rhs.z)
        /// lhs is Expr
        /// rhs is WorkgroupSize [Optional]
        attr_workgroup_size,
        /// @workgroup(lhs, rhs)
        /// lhs is InterpolationType   [TokenIndex]
        /// rhs is InterpolationSample [TokenIndex] [Optional]
        attr_interpolate,

        // ********* Operators *********
        // main_token is operator
        /// lhs * rhs
        mul,
        /// lhs / rhs
        div,
        /// lhs % rhs
        mod,
        /// lhs + rhs
        add,
        /// lhs - rhs
        sub,
        /// lhs << rhs
        shift_left,
        /// lhs >> rhs
        shift_right,
        /// lhs & rhs
        binary_and,
        /// lhs | rhs
        binary_or,
        /// lhs ^ rhs
        binary_xor,
        /// lhs && rhs
        circuit_and,
        /// lhs || rhs
        circuit_or,
        /// !lhs
        not,
        /// -lhs
        negate,
        /// *lhs
        deref,
        /// &lhs
        addr_of,
        /// lhs == rhs
        equal,
        /// lhs != rhs
        not_equal,
        /// lhs < rhs
        less,
        /// lhs <= rhs
        less_equal,
        /// lhs >= rhs
        greater,
        /// lhs >= rhs
        greater_equal,

        // ********* Expressions *********
        /// vec2<f32>(2)
        /// main_token is identifier, 'array', ScalarType, VectorPrefix or MatrixPrefix.
        /// lhs is constructor type [Optional]
        /// rhs is ArgumentExprList
        call_expr,
        /// bitcast<f32>(5)
        /// main_token is 'bitcast'
        /// lhs is destination type
        /// rhs is Expr
        bitcast_expr,
        /// some_ident
        /// main_token is identifier
        ident_expr,

        // ********* Literals *********
        /// main_token is 'true' or 'false'
        bool_literal,
        /// main_token is a number litreal
        number_literal,
    };

    pub const PtrType = struct {
        addr_space: TokenIndex,
        access_mode: TokenIndex = null_node,
    };

    pub const VarDecl = struct {
        attrs: Index = null_node,
        addr_space: TokenIndex = null_node,
        access_mode: TokenIndex = null_node,
        type: Index = null_node,
    };

    pub const WorkgroupSize = struct {
        y: Index,
        z: Index = null_node,
    };
};

pub const BuiltinValue = enum {
    vertex_index,
    instance_index,
    position,
    front_facing,
    frag_depth,
    local_invocation_id,
    local_invocation_index,
    global_invocation_id,
    workgroup_id,
    num_workgroups,
    sample_index,
    sample_mask,
};

pub const InterpolationType = enum {
    perspective,
    linear,
    flat,
};

pub const InterpolationSample = enum {
    center,
    centroid,
    sample,
};

pub const AddressSpace = enum {
    function,
    private,
    workgroup,
    uniform,
    storage,
};

pub const AccessMode = enum {
    read,
    write,
    read_write,
};

pub const Attribute = enum {
    invariant,
    @"const",
    vertex,
    fragment,
    compute,
    @"align",
    binding,
    group,
    id,
    location,
    size,
    builtin,
    workgroup_size,
    interpolate,
};

const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

test Parser {
    std.testing.refAllDeclsRecursive(Parser);
}

test "no errors" {
    const source =
        \\;
        \\@interpdlate(flat) var<storage,> expr = vec3<f32>(1, 5) + 5 - 9 * 7 / 3 & 6;
    ** 1;

    var ast = try parse(std.testing.allocator, source, null);
    defer ast.deinit(std.testing.allocator);
}

// test "no errors" {
//     const t = std.time.microTimestamp() * std.time.ns_per_us;
//     const source =
//         \\;
//         \\@interpolate(flat) var expr = vec3<f32>(1, 5);
//         \\var<storage> expr = bitcast<f32>(5);
//         \\var expr;
//         \\var expr = bool();
//         \\var expr = ~(-(!false));
//         \\var expr = expr;
//         \\var expr = expr(expr);
//         \\const hello = 1;
//         \\override hello;
//         \\type the_type = ptr<workgroup, f32, read>;
//         \\type the_type = array<f32, expr>;
//         \\type the_type = vec3<f32>;
//         \\type the_type = mat2x3<f32>;
//         \\type the_type = atomic<u32>;
//         \\type the_type = sampler;
//         \\struct S {
//         \\  s: u32,
//         \\}
//         \\const_assert 2 > 1;
//         \\fn foo(f: u32) -> u32 {}
//         \\fn foo(f: u32) -> u32 {
//         \\    loop {
//         \\        continuing {
//         \\            continue;
//         \\            break;
//         \\            break if true;
//         \\        }
//         \\        return bar;
//         \\        return;
//         \\        const_assert 2 > 1;
//         \\    }
//         \\}
//     ** 1;

//     var ast = try parse(std.testing.allocator, source, null);
//     defer ast.deinit(std.testing.allocator);
//     std.debug.print("\n{s}\n", .{std.fmt.fmtDurationSigned(std.time.microTimestamp() * std.time.ns_per_us - t)});
// }

test "variable & expressions" {
    const source =
        \\var expr = 1 + 5 + 2 * 3 > 6 >> 7;
    ;

    var ast = try parse(std.testing.allocator, source, null);
    defer ast.deinit(std.testing.allocator);

    const root = ast.nodes.get(0);
    try expect(root.lhs + 1 == root.rhs);

    const @"var expr = 1 + 5 + 2 * 3 > 6 >> 7" = ast.nodes.get(ast.extra_data.items[root.lhs]);
    try expect(@"var expr = 1 + 5 + 2 * 3 > 6 >> 7".tag == .global_variable);
    try expect(ast.tokens[@"var expr = 1 + 5 + 2 * 3 > 6 >> 7".main_token].tag == .keyword_var);

    const @"1 + 5 + 2 * 3 > 6 >> 7" = ast.nodes.get(@"var expr = 1 + 5 + 2 * 3 > 6 >> 7".rhs);
    try expect(@"1 + 5 + 2 * 3 > 6 >> 7".tag == .greater);

    const @"1 + 5 + 2 * 3" = ast.nodes.get(@"1 + 5 + 2 * 3 > 6 >> 7".lhs);
    try expect(@"1 + 5 + 2 * 3".tag == .add);

    const @"1 + 5" = ast.nodes.get(@"1 + 5 + 2 * 3".lhs);
    try expect(@"1 + 5".tag == .add);

    const @"1" = ast.nodes.get(@"1 + 5".lhs);
    try expect(@"1".tag == .number_literal);

    const @"5" = ast.nodes.get(@"1 + 5".rhs);
    try expect(@"5".tag == .number_literal);

    const @"2 * 3" = ast.nodes.get(@"1 + 5 + 2 * 3".rhs);
    try expect(@"2 * 3".tag == .mul);

    const @"6 >> 7" = ast.nodes.get(@"1 + 5 + 2 * 3 > 6 >> 7".rhs);
    try expect(@"6 >> 7".tag == .shift_right);

    const @"6" = ast.nodes.get(@"6 >> 7".lhs);
    try expect(@"6".tag == .number_literal);

    const @"7" = ast.nodes.get(@"6 >> 7".rhs);
    try expect(@"7".tag == .number_literal);
}

// test "type alias" {
//     const source =
//         \\type my_type = vec3<f32>;
//     ;

//     var ast = try parse(std.testing.allocator, source, null);
//     defer ast.deinit(std.testing.allocator);

//     const vec3 = ast.getType(ast.getGlobal(0).type_alias.type);
//     const vec3_elements_type = ast.getType(vec3.vector.element);

//     try expect(vec3.vector.prefix == .vec3);
//     try expect(vec3_elements_type.scalar == .f32);
// }

// test "struct" {
//     const source =
//         \\struct S { s: u32 }
//     ;

//     var ast = try parse(std.testing.allocator, source, null);
//     defer ast.deinit(std.testing.allocator);

//     const strct = ast.getGlobal(0).@"struct";
//     const strct_member_s = strct.members[0];
//     const strct_member_s_type = ast.getType(strct_member_s.type).scalar;

//     try expect(strct.members.len == 1);
//     try expectEqualStrings("S", strct.name);
//     try expectEqualStrings("s", strct_member_s.name);
//     try expect(strct_member_s_type == .u32);
// }

// test "research" {
//     if (true) return error.SkipZigTest;

//     const source =
//         \\type my_type = array<f32, sampler()>;
//     ;

//     var ast = try parse(std.testing.allocator, source, null);
//     defer ast.deinit(std.testing.allocator);

//     const vec3 = ast.getType(ast.getGlobal(0).type_alias.type);
//     const vec3_elements_type = ast.getType(vec3.vector.element);

//     try expect(vec3.vector.prefix == .vec3);
//     try expect(vec3_elements_type.scalar == .f32);
// }
