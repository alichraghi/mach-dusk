const std = @import("std");
const Token = @import("Token.zig");

const Ast = @This();

nodes: std.MultiArrayList(Node) = .{},
extra_data: std.ArrayListUnmanaged(Node.Index) = .{},
scratch: std.ArrayListUnmanaged(Node.Index) = .{},
tokens: []const Token = &.{},

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    self.nodes.deinit(allocator);
    self.extra_data.deinit(allocator);
    self.scratch.deinit(allocator);
    allocator.free(self.tokens);
}

pub const Range = struct {
    start: u32,
    end: u32,
};

pub const TokenIndex = u32;

pub const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
    lhs: Index = null_node,
    rhs: Index = null_node,

    pub const Index = u32;
    pub const null_node: Index = 0;

    pub const Tag = enum {
        /// a helper node pointing at extra_data[lhs..rhs].
        span,
        /// main_token is 'var'.
        /// lhs is a VarDecl.
        /// rhs is initializer, if any.
        global_variable,
        /// main_token is AddressSpace.
        addr_space,
        /// main_token is AccessMode.
        access_mode,
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
        /// rhs is array size, if any.
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
        attr_invariant,
        attr_const,
        attr_vertex,
        attr_fragment,
        attr_compute,
        // @attr(lhs)
        // lhs is Expr
        attr_align,
        attr_binding,
        attr_group,
        attr_id,
        attr_location,
        attr_size,
        /// @builtin(lhs)
        /// lhs is BuiltinValue
        attr_builtin,
        /// @workgroup(lhs, rhs.y, rhs.z)
        /// lhs is Expr.
        /// rhs is Workgroup if any.
        attr_workgroup,
        /// @workgroup(lhs, rhs)
        /// lhs is InterpolationType
        /// rhs is InterpolationSample, if any.
        attr_interpolate,

        variable_qualifier,

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

        call_expr,
        bitcast_expr,
        ident_expr,

        bool_literal,
        number_literal,
    };

    pub const PtrType = struct {
        addr_space: Index,
        access_mode: Index = null_node,
    };

    pub const VarDecl = struct {
        attrs: Index = null_node,
        addr_space: Index = null_node,
        access_mode: Index = null_node,
        type: Index = null_node,
    };

    pub const Workgroup = struct {
        y: Index,
        z: Index = null_node,
    };
};
