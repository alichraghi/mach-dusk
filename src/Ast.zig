const std = @import("std");
const Token = @import("Token.zig");

const Ast = @This();

/// TODO: make these MultiArrayList
/// Entry of the parse result
globals: std.ArrayListUnmanaged(GlobalDecl) = .{},
/// Contains expression's that a GlobalDecl in `globals` may need
expressions: std.ArrayListUnmanaged(Expression) = .{},
/// Contains Type's that an ArrayType `element_type` field need
types: std.ArrayListUnmanaged(Type) = .{},

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    self.globals.deinit(allocator);
    self.expressions.deinit(allocator);
    self.types.deinit(allocator);
}

pub fn Index(comptime _: type) type {
    return u32;
}

pub fn Range(comptime _: type) type {
    return struct {
        start: u32,
        end: u32,
    };
}

pub const GlobalDecl = union(enum) {
    // Workaround: https://github.com/ziglang/zig/issues/12781
    // variable: GlobalVariable,
    // function: Function,
    // constant: Const,
    // structure: Struct,
    type_alias: TypeAlias,
};

pub const GlobalVariable = struct {};

pub const Function = struct {};

pub const Const = struct {};

pub const Struct = struct {};

pub const Expression = union(enum) {
    literal: Literal,
    construct: ConstructExpr,
    unary_operator: struct {
        op: UnaryOperator,
        expr: Index(Expression),
    },
    addr_of: Index(Expression),
    deref: Index(Expression),
    binary: struct {
        op: BinaryOperator,
        left: Index(Expression),
        right: Index(Expression),
    },
    call: struct {
        function: []const u8,
        arguments: Range(Expression),
    },
    index: struct {
        base: Index(Expression),
        index: Index(Expression),
    },
    member: struct {
        base: Index(Expression),
        field: []const u8,
    },
    bitcast: struct {
        expr: Index(Expression),
        to: Type,
        type_name: []const u8,
    },
};

pub const ConstructExpr = struct {
    type: union(enum) {
        /// f32(1.0)
        scalar: ScalarType,
        /// vec3(1.0)
        partial_vector: VectorType.Prefix,
        /// vec3<f32>(1.0)
        vector: VectorType,
        /// mat2x2(1,2,3,4)
        partial_matrix: MatrixType.Prefix,
        /// mat2x2<f32>(1,2,3,4)
        matrix: MatrixType,
        /// array(1, 2, 3)
        partial_array,
        /// array<i32>(1, 2, 3)
        array: ArrayType,
        /// type my_vec = vec3<f32>;
        /// my_vec(1.0)
        user: []const u8,
    },
    components: Range(Expression),
};

pub const Literal = union(enum) {
    number: Number,
    bool: bool,
};

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

pub const UnaryOperator = enum {
    negate,
    not,
};

pub const BinaryOperator = enum {
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
    /// &
    @"and",
    /// ^
    exclusive_or,
    /// |
    inclusive_or,
    /// &&
    logical_and,
    /// ||
    logical_or,
    /// <<
    shift_left,
    /// >>
    shift_right,
};

pub const TypeAlias = struct {
    name: []const u8,
    type: Type,
};

pub const Type = union(enum) {
    scalar: ScalarType,
    vector: VectorType,
    matrix: MatrixType,
    sampler: SamplerType,
    atomic: AtomicType,
    array: ArrayType,
    /// A user-defined type, like a struct or a type alias.
    user: []const u8,
};

pub const ScalarType = enum {
    i32,
    u32,
    f32,
    f16,
    bool,

    pub fn size(self: ScalarType) u8 {
        return switch (self) {
            .i32, .u32, .f32 => 4,
            .f16 => 2,
            .bool => 1,
        };
    }
};

pub const SamplerType = struct {
    comparison: bool,
};

pub const VectorType = struct {
    pub const Prefix = Size;
    pub const Size = enum {
        bi,
        tri,
        quad,

        pub fn len(self: Size) u5 {
            return switch (self) {
                .bi => 2,
                .tri => 3,
                .quad => 4,
            };
        }
    };

    size: Size,
    element_type: ScalarType,
};

pub const MatrixType = struct {
    pub const Prefix = struct {
        rows: VectorType.Size,
        columns: VectorType.Size,
    };

    rows: VectorType.Size,
    columns: VectorType.Size,
    element_type: ScalarType,
};

pub const AtomicType = struct {
    element_type: ScalarType,
};

pub const ArrayType = struct {
    pub const Size = union(enum) {
        static: Index(Expression),
        dynamic,
    };

    size: Size,
    element_type: Index(Type),
};
