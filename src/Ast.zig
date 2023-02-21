const std = @import("std");
const Token = @import("Token.zig");

const Ast = @This();

nodes: std.MultiArrayList(Node) = .{},
extra_data: std.ArrayListUnmanaged(Node.Index) = .{},
scratch: std.ArrayListUnmanaged(Node.Index) = .{},

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    self.nodes.deinit(allocator);
    self.extra_data.deinit(allocator);
    self.scratch.deinit(allocator);
}

pub const Range = struct {
    start: u32,
    end: u32,
};

pub const Node = struct {
    tag: Tag,
    main_token: Token,
    lhs: Index = null_node,
    rhs: Index = null_node,

    pub const Index = u32;
    pub const null_node: Index = 0;

    pub const Tag = enum {
        span,

        global_variable,
        addr_space,
        access_mode,
        scalar_type,
        sampler_type,
        vector_type,
        matrix_type,
        atomic_type,
        array_type,
        ptr_type,
        user_type,

        /// main_token: '@'
        attr_invariant,
        attr_const,
        attr_vertex,
        attr_fragment,
        attr_compute,
        /// lhs: Expr
        attr_align,
        attr_binding,
        attr_group,
        attr_id,
        attr_location,
        attr_size,
        /// lhs: BuiltinValue
        attr_builtin,
        /// lhs: ExprX
        /// rhs: Workgroup(optional)
        attr_workgroup,
        /// lhs: InterpolationType
        /// rhs: InterpolationSample(optional)
        attr_interpolate,

        builtin_value,
        interpolation_type,
        interpolation_sample,

        variable_qualifier,

        mul,
        div,
        mod,
        add,
        sub,
        shift_left,
        shift_right,
        binary_and,
        binary_or,
        binary_xor,
        circuit_and,
        circuit_or,
        not,
        negate,
        deref,
        addr_of,
        equal,
        not_equal,
        less,
        less_equal,
        greater,
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
