const std = @import("std");
const Token = @import("Token.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Resolver = @import("Resolver.zig");

const Ast = @This();
pub const Index = u32;

allocator: std.mem.Allocator,
source: [:0]const u8,
tokens: std.ArrayListUnmanaged(Token) = .{},
nodes: std.MultiArrayList(Node) = .{},
extra: std.ArrayListUnmanaged(Index) = .{},

pub fn getToken(self: Ast, i: Index) Token {
    return self.tokens.items[std.math.min(i, self.tokens.items.len)];
}

/// parses TranslationUnit(WGSL Program)
pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) !Ast {
    var p = Parser{
        .allocator = allocator,
        .ast = .{ .allocator = allocator, .source = source },
        .error_list = .{ .allocator = allocator, .source = source },
    };

    var tokenizer = Tokenizer.init(source);
    const estimated_tokens = source.len / 8;
    try p.ast.tokens.ensureTotalCapacity(allocator, estimated_tokens);
    while (true) {
        const t = tokenizer.next();
        try p.ast.tokens.append(allocator, t);
        if (t.tag == .eof) break;
    }

    defer {
        p.scratch.deinit(allocator);
        p.error_list.deinit();
    }
    errdefer p.ast.deinit();

    const estimated_nodes = (p.ast.tokens.items.len + 2) / 2;
    try p.ast.nodes.ensureTotalCapacity(allocator, estimated_nodes);

    const root = p.addNode(.{
        .tag = .span,
        .main_token = undefined,
    }) catch unreachable;

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.peekToken(0).tag != .eof) {
        const decl = try p.expectGlobalDeclRecoverable() orelse continue;
        try p.scratch.append(allocator, decl);
    }

    if (p.error_list.errors.items.len > 0) {
        try p.error_list.flush();
        return error.Parsing;
    }

    const list = p.scratch.items[scratch_top..];
    try p.ast.extra.appendSlice(allocator, list);
    p.ast.nodes.items(.lhs)[root] = @intCast(Ast.Index, p.ast.extra.items.len - list.len);
    p.ast.nodes.items(.rhs)[root] = @intCast(Ast.Index, p.ast.extra.items.len);

    // resolve
    try Resolver.resolve(allocator, p.ast);

    return p.ast;
}

pub fn deinit(self: *Ast) void {
    self.nodes.deinit(self.allocator);
    self.extra.deinit(self.allocator);
    self.tokens.deinit(self.allocator);
}

pub const null_index: Index = 0;
pub const Node = struct {
    tag: Tag,
    main_token: Index,
    lhs: Index = null_index,
    rhs: Index = null_index,

    pub const Tag = enum {
        /// an slice to extra field [LHS..RHS]
        /// TOK : undefined
        /// LHS : Index
        /// RHS : Index
        span,

        // ####### GlobalDecl #######

        /// TOK : k_var
        /// LHS : GlobalVarDecl
        /// RHS : Expr?
        global_variable,

        /// TOK : k_const
        /// LHS : Type
        /// RHS : Expr
        global_constant,

        /// TOK : k_override
        /// LHS : OverrideDecl
        /// RHS : Expr
        override,

        /// TOK : k_type
        /// LHS : Type
        /// RHS : --
        type_alias,

        /// TOK : k_const_assert
        /// LHS : Expr
        /// RHS : --
        const_assert,

        /// TOK : k_struct
        /// LHS : span(struct_member)
        /// RHS : --
        struct_decl,
        /// TOK : ident
        /// LHS : span(Attribute)
        /// RHS : Type
        struct_member,

        /// TOK : k_fn
        /// LHS : FnProto
        /// RHS : block
        fn_decl,
        /// TOK : ident
        /// LHS : ? Attributes
        /// RHS : type
        fn_param,

        // ####### Statement #######

        /// TOK : k_return
        /// LHS : Expr?
        /// RHS : --
        return_statement,

        /// TOK : k_discard
        /// LHS : --
        /// RHS : --
        discard_statement,

        /// TOK : k_loop
        /// LHS : block
        /// RHS : --
        loop_statement,

        /// TOK : k_continuing
        /// LHS : block
        /// RHS : --
        continuing_statement,

        /// TOK : k_break
        /// LHS : Expr
        /// RHS : --
        break_if_statement,

        /// TOK : k_break
        /// LHS : --
        /// RHS : --
        break_statement,

        /// TOK : k_continue
        /// LHS : --
        /// RHS : --
        continue_statement,

        /// TOK : k_if
        /// LHS : Expr
        /// RHS : blcok
        if_statement,
        /// RHS is else body
        /// TOK : k_if
        /// LHS : if_statement
        /// RHS : blcok
        if_else_statement,
        /// TOK : k_if
        /// LHS : if_statement
        /// RHS : if_statement, if_else_statement, if_else_if_statement
        if_else_if_statement,

        /// TOK : k_switch
        /// LHS : Expr
        /// RHS : span(switch_case, switch_default, switch_case_default)
        switch_statement,
        /// TOK : k_case
        /// LHS : span(Expr)
        /// RHS : block
        switch_case,
        /// TOK : k_default
        /// LHS : block
        /// RHS : --
        switch_default,
        /// switch_case with default (`case 1, 2, default {}`)
        /// TOK : k_case
        /// LHS : span(Expr)
        /// RHS : block
        switch_case_default,

        /// TOK : k_var
        /// LHS : VarDecl
        /// RHS : Expr?
        var_decl,

        /// TOK : k_const
        /// LHS : Type?
        /// RHS : Expr
        const_decl,

        /// TOK : k_let
        /// LHS : Type?
        /// RHS : Expr
        let_decl,

        /// TOK : k_while
        /// LHS : Expr
        /// RHS : block
        while_statement,

        /// TOK : k_for
        /// LHS : ForHeader
        /// RHS : block
        for_statement,

        /// TOK : plus_plus, minus_minus
        /// LHS : Expr
        increase_decrement_statement,

        /// TOK : plus_equal,        minus_equal,
        ///       times_equal,       division_equal,
        ///       modulo_equal,      and_equal,
        ///       or_equal,          xor_equal,
        ///       shift_right_equal, shift_left_equal
        /// LHS : Expr
        /// RHS : Expr
        compound_assign_statement,

        /// TOK : equal
        /// LHS : Expr
        /// RHS : --
        phony_assign_statement,

        // ####### Type #######

        /// TOK : k_i32, k_u32, k_f32, k_f16, k_bool
        /// LHS : --
        /// RHS : --
        scalar_type,

        /// TOK : k_sampler, k_comparison_sampler
        /// LHS : --
        /// RHS : --
        sampler_type,

        /// TOK : k_vec2, k_vec3, k_vec4
        /// LHS : Type
        /// RHS : --
        vector_type,

        /// TOK : k_mat2x2, k_mat2x3, k_mat2x4,
        ///       k_mat3x2, k_mat3x3, k_mat3x4,
        ///       k_mat4x2, k_mat4x3, k_mat4x4
        /// LHS : Type
        /// RHS : --
        matrix_type,

        /// TOK : k_atomic
        /// LHS : Type
        /// RHS : --
        atomic_type,

        /// TOK : k_array
        /// LHS : Type
        /// RHS : Expr?
        array_type,

        /// TOK : k_ptr
        /// LHS : Type
        /// RHS : PtrType
        ptr_type,

        /// TOK : ident
        /// LHS : --
        /// RHS : --
        user_type,

        // ####### Attribute #######

        // TOK : attr
        attr,

        /// TOK : attr
        /// LHS : Expr
        /// RHS : --
        attr_one_arg,

        /// TOK : attr
        /// LHS : Index(Token(BuiltinValue))
        /// RHS : --
        attr_builtin,

        /// TOK : attr
        /// LHS : WorkgroupSize
        /// RHS : --
        attr_workgroup_size,

        /// TOK : attr
        /// LHS : Index(Token(InterpolationType))
        /// RHS : Index(Token(InterpolationSample))
        attr_interpolate,

        // ####### Expr #######

        /// TOK : *
        /// LHS : --
        /// RHS : --
        mul,

        /// TOK : /
        /// LHS : --
        /// RHS : --
        div,

        /// TOK : %
        /// LHS : --
        /// RHS : --
        mod,

        /// TOK : +
        /// LHS : --
        /// RHS : --
        add,

        /// TOK : -
        /// LHS : --
        /// RHS : --
        sub,

        /// TOK : <<
        /// LHS : --
        /// RHS : --
        shift_left,

        /// TOK : >>
        /// LHS : --
        /// RHS : --
        shift_right,

        /// TOK : &
        /// LHS : --
        /// RHS : --
        binary_and,

        /// TOK : |
        /// LHS : --
        /// RHS : --
        binary_or,

        /// TOK : ^
        /// LHS : --
        /// RHS : --
        binary_xor,

        /// TOK : &&
        /// LHS : --
        /// RHS : --
        circuit_and,

        /// TOK : ||
        /// LHS : --
        /// RHS : --
        circuit_or,

        /// TOK : !
        /// LHS : --
        /// RHS : --
        not,

        /// TOK : -
        /// LHS : --
        /// RHS : --
        negate,

        /// TOK : *
        /// LHS : --
        /// RHS : --
        deref,

        /// TOK : &
        /// LHS : --
        /// RHS : --
        addr_of,

        /// TOK : ==
        /// LHS : --
        /// RHS : --
        equal,

        /// TOK : !=
        /// LHS : --
        /// RHS : --
        not_equal,

        /// TOK : <
        /// LHS : --
        /// RHS : --
        less,

        /// TOK : <=
        /// LHS : --
        /// RHS : --
        less_equal,

        /// TOK : >
        /// LHS : --
        /// RHS : --
        greater,

        /// TOK : >=
        /// LHS : --
        /// RHS : --
        greater_equal,

        /// for identifier, array without element type specified,
        /// vector prefix (e.g. vec2) and matrix prefix (e.g. mat2x2) LHS is null
        /// see callExpr in Parser.zig if you don't understand this
        ///
        /// TOK : ident, k_array, 'scalar keywords', 'vector keywords', 'matrix keywords'
        /// LHS : (scalar_type, vector_type, matrix_type, array_type)?
        /// RHS : arguments (Expr span)
        call_expr,

        /// TOK : k_bitcast
        /// LHS : Type
        /// RHS : Expr
        bitcast_expr,

        /// TOK : ident
        /// LHS : --
        /// RHS : --
        ident_expr,

        /// TOK : '*'
        /// LHS :  Expr
        deref_expr,

        /// TOK : '&'
        /// LHS :  Expr
        addr_of_expr,

        /// LHS is prefix expression
        /// TOK : ident
        /// LHS : Expr
        component_access,

        /// LHS is prefix expression
        /// TOK : bracket_left
        /// LHS : Expr
        /// RHS : Expr
        index_access,

        // ####### Literals #######

        /// TOK : k_true, k_false
        /// LHS : --
        /// RHS : --
        bool_literal,
        /// TOK : number
        /// LHS : --
        /// RHS : --
        number_literal,
    };

    pub const GlobalVarDecl = struct {
        attrs: Index = null_index,
        name: Index, // Token
        addr_space: Index = null_index, // Token
        access_mode: Index = null_index, // Token
        type: Index = null_index,
    };

    pub const VarDecl = struct {
        name: Index, // Token
        addr_space: Index = null_index, // Token
        access_mode: Index = null_index, // Token
        type: Index = null_index,
    };

    pub const OverrideDecl = struct {
        attrs: Index = null_index,
        type: Index = null_index,
    };

    pub const PtrType = struct {
        addr_space: Index, // Token
        access_mode: Index = null_index, // Token
    };

    pub const WorkgroupSize = struct {
        x: Index,
        y: Index = null_index,
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
