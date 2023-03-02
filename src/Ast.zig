const std = @import("std");
const Token = @import("Token.zig");
const Tokenizer = @import("Tokenizer.zig");
const TokenList = @import("TokenList.zig");
const Parser = @import("Parser.zig");
const Resolver = @import("Resolver.zig");

const Ast = @This();

nodes: std.MultiArrayList(Node) = .{},
extra_data: std.ArrayListUnmanaged(Node.Index) = .{},
tokens: TokenList,
source: [:0]const u8,

/// parses a TranslationUnit(WGSL Program)
pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) !Ast {
    var tokenizer = Tokenizer.init(source);
    const estimated_tokens = source.len / 8;
    var tokens = try std.ArrayList(Token).initCapacity(allocator, estimated_tokens);
    while (true) {
        const t = tokenizer.next();
        try tokens.append(t);
        if (t.tag == .eof) break;
    }

    var p = Parser{
        .allocator = allocator,
        .ast = .{
            .tokens = .{ .list = try tokens.toOwnedSlice() },
            .source = source,
        },
        .error_list = .{ .allocator = allocator, .source = source },
    };
    defer {
        p.scratch.deinit(allocator);
        p.error_list.deinit();
    }
    errdefer p.ast.deinit(allocator);

    const estimated_nodes = (p.ast.tokens.list.len + 2) / 2;
    try p.ast.nodes.ensureTotalCapacity(allocator, estimated_nodes);

    const root = p.addNode(.{
        .tag = .span,
        .main_token = undefined,
    }) catch unreachable;

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.ast.tokens.peek(0).tag != .eof) {
        const decl = try p.expectGlobalDeclRecoverable() orelse continue;
        try p.scratch.append(allocator, decl);
    }

    if (p.error_list.errors.items.len > 0) {
        try p.error_list.flush();
        return error.Parsing;
    }

    const list = p.scratch.items[scratch_top..];
    try p.ast.extra_data.appendSlice(allocator, list);
    p.ast.nodes.items(.lhs)[root] = @intCast(Ast.Node.Index, p.ast.extra_data.items.len - list.len);
    p.ast.nodes.items(.rhs)[root] = @intCast(Ast.Node.Index, p.ast.extra_data.items.len);

    // resolve
    try Resolver.resolve(allocator, p.ast);

    return p.ast;
}

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    self.nodes.deinit(allocator);
    self.extra_data.deinit(allocator);
    allocator.free(self.tokens.list);
}

pub const null_index: Node.Index = 0;
pub const Node = struct {
    tag: Tag,
    main_token: TokenList.Index,
    lhs: Index = null_index,
    rhs: Index = null_index,

    pub const Index = u32;

    pub const Tag = enum {
        /// a helper node pointing at extra_data[lhs..rhs]
        span,

        // ********* Global declarations *********
        /// main_token is 'var'
        /// lhs is a GlobalVarDecl
        /// rhs is initializer expression [Optional]
        global_variable,
        /// main_token is 'const'
        /// lhs is type
        /// rhs is initializer expression
        global_constant,
        /// main_token is 'override'
        /// lhs is GlobalOverrideDecl
        /// rhs is initializer expression
        global_override,
        /// main_token is 'type'
        /// lhs is a type
        type_alias,
        /// main_token is 'const_assert'
        /// lhs is an expression
        const_assert,
        /// main_token is 'struct'
        /// lhs is struct members span
        struct_decl,
        /// main_token is member name (identifier)
        /// lhs is attributes span
        /// rhs is type
        struct_member,
        /// main_token is 'fn'
        /// lhs is FnProto
        /// rhs is body
        fn_decl,
        /// main_token is param name
        /// lhs is Attributes [Optional]
        /// rhs is type
        fn_param,

        // ********* Statements *********
        /// main_token is 'return'
        /// lhs is expression [Optional]
        return_statement,
        /// main_token is 'discard'
        discard_statement,
        /// main_token is 'loop'
        /// lhs is body block
        loop_statement,
        /// main_token is 'continuing'
        /// lhs is body block
        continuing_statement,
        /// main_token is 'break'
        /// lhs is condition expression
        break_if_statement,
        /// main_token is 'break'
        break_statement,
        /// main_token is 'continue'
        continue_statement,
        /// main_token is 'if'
        /// lhs is condition
        /// rhs is body
        if_statement,
        /// main_token is 'if'
        /// lhs is IfStatement
        /// rhs is else body
        if_else_statement,
        /// main_token is 'if'
        /// lhs is IfStatement
        /// rhs is if_statement, if_else_statement or if_else_if_statement
        if_else_if_statement,
        /// switch lhs { rhs }
        /// main_token is 'switch'
        /// lhs is expression
        /// rhs is cases span
        switch_statement,
        /// main_token is 'default'
        /// lhs is body block
        switch_default,
        /// case lhs { rhs }
        /// main_token is 'case'
        /// lhs is cases span
        /// lhs is body block
        switch_case,
        /// case lhs, default { rhs }
        /// main_token is 'case'
        /// lhs is cases span
        /// lhs is body block
        switch_case_default,
        /// main_token is 'var'
        /// lhs is a GlobalVarDecl
        /// rhs is initializer expression [Optional]
        var_decl,
        /// main_token is 'const'
        /// lhs is a type [Optional]
        /// rhs is initializer expression
        const_decl,
        /// main_token is 'let'
        /// lhs is a type [Optional]
        /// rhs is initializer expression
        let_decl,
        /// main_token is 'while'
        /// lhs is condition expression
        /// rhs is body block
        while_statement,
        /// main_token is 'for'
        /// lhs is ForHeader
        /// rhs is body block
        for_statement,
        /// main_token is '++' or '--'
        /// lhs is expression
        increase_decrement_statement,
        /// main_token is compound assignment operator
        /// lhs is expression
        /// rhs is expression
        compound_assign_statement,
        /// main_token is '='
        /// lhs is expression
        phony_assign_statement,

        // ********* Types *********
        /// main_token is ScalarType
        scalar_type,
        /// main_token is SamplerType
        sampler_type,
        /// vec2<lhs>
        /// main_token is VectorPrefix
        /// lhs is element type
        vector_type,
        /// mat2x2<lhs>
        /// main_token is MatrixPrefix
        /// lhs is element type
        matrix_type,
        /// atomic<lhs>
        /// main_token is 'atomic'
        /// lhs is element type
        atomic_type,
        /// array<lhs, rhs>
        /// main_token is 'array'
        /// lhs is element type
        /// rhs is array size expression [Optional]
        array_type,
        /// ptr<rhs.addr_space, lhs, rhs.access_mode>
        /// main_token is 'ptr'
        /// lhs is element type
        /// rhs is PtrType
        /// rhs.access_mode is [Optional]
        ptr_type,
        /// main_token is identifier
        user_type,

        // ********* Attributes *********
        // main_token is '@'
        /// @const
        attr,
        /// @align(lhs)
        /// lhs is an expression
        attr_one_arg,
        /// @builtin(lhs)
        /// lhs is [TokenList.Index]
        attr_builtin,
        /// @workgroup_size(lhs, rhs.y, rhs.z)
        /// lhs is an expression
        /// rhs is WorkgroupSize [Optional]
        attr_workgroup_size,
        /// @workgroup(lhs, rhs)
        /// lhs is InterpolationType   [TokenList.Index]
        /// rhs is InterpolationSample [TokenList.Index] [Optional]
        attr_interpolate,

        // ********* Expressions *********
        /// lhs * rhs
        /// main_token is *
        mul,
        /// lhs / rhs
        /// main_token is /
        div,
        /// lhs % rhs
        /// main_token is %
        mod,
        /// lhs + rhs
        /// main_token is +
        add,
        /// lhs - rhs
        /// main_token is -
        sub,
        /// lhs << rhs
        /// main_token is <<
        shift_left,
        /// lhs >> rhs
        /// main_token is >>
        shift_right,
        /// lhs & rhs
        /// main_token is &
        binary_and,
        /// lhs | rhs
        /// main_token is |
        binary_or,
        /// lhs ^ rhs
        /// main_token is ^
        binary_xor,
        /// lhs && rhs
        /// main_token is &&
        circuit_and,
        /// lhs || rhs
        /// main_token is ||
        circuit_or,
        /// !lhs
        /// main_token is !
        not,
        /// -lhs
        /// main_token is -
        negate,
        /// *lhs
        /// main_token is *
        deref,
        /// &lhs
        /// main_token is &
        addr_of,
        /// lhs == rhs
        /// main_token is ==
        equal,
        /// lhs != rhs
        /// main_token is !=
        not_equal,
        /// lhs < rhs
        /// main_token is <
        less,
        /// lhs <= rhs
        /// main_token is <=
        less_equal,
        /// lhs > rhs
        /// main_token is >
        greater,
        /// lhs >= rhs
        /// main_token is >=
        greater_equal,
        /// vec2<f32>(2)
        /// main_token is an identifier or
        /// type constructor ('array', ScalarType, VectorPrefix or MatrixPrefix).
        ///
        /// lhs is TypeSpecifierWithoutIdent [Optional]
        /// rhs is arguments (expression span)
        call_expr,
        /// bitcast<f32>(5)
        /// main_token is 'bitcast'
        /// lhs is destination type
        /// rhs is an expression
        bitcast_expr,
        /// main_token is an identifier
        ident_expr,
        /// *lhs
        /// main_token is '*'
        /// lhs is expression
        deref_expr,
        /// &lhs
        /// main_token is '&'
        /// lhs is expression
        addr_of_expr,
        /// lhs.some_ident
        /// main_token is identifier
        /// lhs is prefix expression
        component_access,
        /// lhs[rhs]
        /// main_token is '['
        /// lhs is prefix expression
        /// rhs is expression
        index_access,

        // ********* Literals *********
        /// main_token is 'true' or 'false'
        bool_literal,
        /// main_token is a number litreal
        number_literal,
    };

    pub const GlobalVarDecl = struct {
        attrs: Index = null_index,
        name: TokenList.Index,
        addr_space: TokenList.Index = null_index,
        access_mode: TokenList.Index = null_index,
        type: Index = null_index,
    };

    pub const VarDecl = struct {
        name: TokenList.Index,
        addr_space: TokenList.Index = null_index,
        access_mode: TokenList.Index = null_index,
        type: Index = null_index,
    };

    pub const GlobalOverrideDecl = struct {
        attrs: Index = null_index,
        type: Index = null_index,
    };

    pub const PtrType = struct {
        addr_space: TokenList.Index,
        access_mode: TokenList.Index = null_index,
    };

    pub const WorkgroupSize = struct {
        y: Index,
        z: Index = null_index,
    };

    pub const FnProto = struct {
        attrs: Index = null_index,
        params: Index = null_index,
        result_attrs: Index = null_index,
        result_type: Index = null_index,
    };

    pub const IfStatement = struct {
        cond: Index,
        body: Index,
    };

    pub const ForHeader = struct {
        init: Index = null_index,
        cond: Index = null_index,
        update: Index = null_index,
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
