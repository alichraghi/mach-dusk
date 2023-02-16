const std = @import("std");
const Token = @import("Token.zig");

const Ast = @This();

/// TODO: make these MultiArrayList
/// Entry of the parse result
globals: std.ArrayListUnmanaged(GlobalDecl) = .{},
/// Contains expression's that a GlobalDecl in `globals` may need
expressions: std.ArrayListUnmanaged(Expression) = .{},
/// Contains index of expression's extra info,
/// like function call arguments expressions
extra: std.ArrayListUnmanaged(Index(Expression)) = .{},
/// Contains Type's that an ArrayType `element_type` field need
types: std.ArrayListUnmanaged(Type) = .{},

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    for (self.globals.items) |global| {
        switch (global) {
            .variable => |variable| {
                allocator.free(variable.attrs);
            },
            else => {},
        }
    }
    self.globals.deinit(allocator);
    self.expressions.deinit(allocator);
    self.extra.deinit(allocator);
    self.types.deinit(allocator);
}

pub fn getGlobal(self: Ast, i: Index(GlobalDecl)) GlobalDecl {
    return self.globals.items[i];
}

pub fn getExpr(self: Ast, i: Index(Expression)) *Expression {
    return &self.expressions.items[i];
}

pub fn getExprRange(self: Ast, r: Range(Expression)) []const Expression {
    return self.expressions.items[r.start..r.end];
}

pub fn getType(self: Ast, i: Index(Type)) Type {
    return self.types.items[i];
}

pub fn Range(comptime _: type) type {
    return struct {
        start: u32,
        end: u32,
    };
}

pub fn Index(comptime _: type) type {
    return u32;
}

pub const GlobalDecl = union(enum) {
    variable: Variable,
    @"const": Const,
    function: Function,
    @"struct": Struct,
    type_alias: TypeAlias,
};

pub const Attribute = union(enum) {
    @"const",
    invariant,
    vertex,
    fragment,
    compute,
    @"align": Index(Expression),
    binding: Index(Expression),
    group: Index(Expression),
    id: Index(Expression),
    location: Index(Expression),
    size: Index(Expression),
    builtin: Builtin,
    interpolate: struct {
        type: InterpolationType,
        sample: ?InterpolationSample,
    },
    workgroup_size: struct {
        Index(Expression),
        ?Index(Expression),
        ?Index(Expression),
    },

    pub const Builtin = enum {
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
};

pub const Variable = struct {
    name: []const u8,
    addr_space: ?AddressSpace,
    access: ?AccessMode,
    type: ?Index(Type),
    value: ?Index(Expression),
    /// allocated
    attrs: []const Attribute,

    pub const DeclInfo = struct {
        ident: OptionalyTypedIdent,
        qualifier: ?Qualifier,
    };

    pub const Qualifier = struct {
        addr_space: AddressSpace,
        access: ?AccessMode,
    };
};

pub const Const = struct {
    name: []const u8,
    type: ?Index(Type),
    value: ?Index(Expression),
};

pub const Override = struct {
    name: []const u8,
    type: ?Index(Type),
    value: ?Index(Expression),
    /// allocated
    attrs: []const Attribute,
};

pub const Function = struct {};

pub const Struct = struct {};

pub const AddressSpace = enum {
    function,
    private,
    storage,
    uniform,
    workgroup,
};

pub const AccessMode = enum {
    read,
    write,
    read_write,
};

pub const TypeAlias = struct {
    name: []const u8,
    type: Index(Type),
};

pub const Expression = union(enum) {
    literal: Literal,
    unary: UnaryExpr,
    binary: BinaryExpr,
    call: CallExpr,
    bitcast: BitcastExpr,
    index: struct { // TODO
        base: Index(Expression),
        index: Index(Expression),
    },
    member: struct { // TODO
        base: Index(Expression),
        field: []const u8,
    },
    ident: []const u8,
};

pub const Literal = union(enum) {
    number: Number,
    bool: bool,

    pub const Number = union(enum) {
        /// Abstract Int (-2^63 ≤ i < 2^63)
        abstract_int: i64,
        /// Abstract Float (IEEE-754 binary64)
        abstract_float: f64,
        /// i32
        i32: i32,
        /// u32
        u32: u32,
        /// f32
        f32: f32,
        /// f16
        f16: f32,
    };
};

pub const CallExpr = struct {
    pub const Callable = union(enum) {
        scalar: Type.Scalar,
        partial_vector: Type.Vector.Prefix,
        vector: Type.Vector,
        partial_matrix: Type.Matrix.Prefix,
        matrix: Type.Matrix,
        partial_array,
        array: Type.Array,
        ident: []const u8,
    };

    callable: Callable,
    args: Range(Ast.Index(Expression)),
};

pub const BitcastExpr = struct {
    /// only i32, u32 and f32 is allowed
    dest: Index(Type),
    expr: Index(Expression),
};

pub const UnaryExpr = struct {
    pub const Operator = enum {
        negate,
        not,
        deref,
        addr_of,
    };

    op: Operator,
    expr: Index(Expression),
};

pub const BinaryExpr = struct {
    pub const Operator = enum {
        /// +
        add,
        /// -
        subtract,
        /// *
        multiply,
        /// /
        divide,
        /// %
        modulo,
        /// ==
        equal,
        /// !=
        not_equal,
        /// <
        less,
        /// <=
        less_equal,
        /// >
        greater,
        /// >=
        greater_equal,
        /// &&
        circuit_and,
        /// ||
        circuit_or,
        /// &
        binary_and,
        /// |
        binary_or,
        /// ^
        binary_xor,
        /// <<
        shift_left,
        /// >>
        shift_right,
    };

    op: Operator,
    left: Index(Expression),
    right: Index(Expression),
};

pub const Type = union(enum) {
    scalar: Scalar,
    vector: Vector,
    matrix: Matrix,
    sampler: Sampler,
    atomic: Atomic,
    array: Array,
    ptr: Pointer,
    /// A user-defined type, like a struct or a type alias.
    user: []const u8,

    pub const Scalar = enum {
        i32,
        u32,
        f32,
        f16,
        bool,

        // pub fn size(self: Scalar) u8 {
        //     return switch (self) {
        //         .i32, .u32, .f32 => 4,
        //         .f16 => 2,
        //         .bool => 1,
        //     };
        // }
    };

    pub const Sampler = struct {
        comparison: bool,
    };

    pub const Vector = struct {
        prefix: Prefix,
        element: Index(Type),

        pub const Prefix = enum {
            vec2,
            vec3,
            vec4,

            pub fn len(self: Prefix) u3 {
                return switch (self) {
                    .vec2 => 2,
                    .vec3 => 3,
                    .vec4 => 4,
                };
            }
        };
    };

    pub const Matrix = struct {
        prefix: Prefix,
        element: Index(Type),

        pub const Prefix = enum {
            mat2x2,
            mat2x3,
            mat2x4,
            mat3x2,
            mat3x3,
            mat3x4,
            mat4x2,
            mat4x3,
            mat4x4,

            pub fn cols(self: Prefix) u3 {
                return switch (self) {
                    .mat2x2, .mat2x3, .mat2x4 => 2,
                    .mat3x2, .mat3x3, .mat3x4 => 3,
                    .mat4x2, .mat4x3, .mat4x4 => 4,
                };
            }

            pub fn rows(self: Prefix) u3 {
                return switch (self) {
                    .mat2x2, .mat3x2, .mat4x2 => 2,
                    .mat2x3, .mat3x3, .mat4x3 => 3,
                    .mat2x4, .mat3x4, .mat4x4 => 4,
                };
            }

            pub fn dimensions(self: Prefix) u6 {
                return self.cols() * self.rows();
            }
        };
    };

    pub const Atomic = struct {
        element: Index(Type),
    };

    pub const Array = struct {
        pub const Size = union(enum) {
            static: Index(Expression),
            dynamic,
        };

        size: Size,
        element: Index(Type),
    };

    pub const FixedArray = struct {
        size: Index(Expression),
        element: Index(Type),
    };

    pub const Pointer = struct {
        addr_space: AddressSpace,
        type: Index(Type),
        access: ?AccessMode,
    };
};

pub const TypedIdent = struct {
    name: []const u8,
    type: Index(Type),
};

pub const OptionalyTypedIdent = struct {
    name: []const u8,
    type: ?Index(Type),
};
