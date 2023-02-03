const std = @import("std");
const Token = @import("Token.zig");

const Ast = @This();

globals: std.ArrayListUnmanaged(GlobalDecl) = .{},

/// Contains expressions referenced in global
expressions: std.ArrayListUnmanaged(Expression) = .{},
/// Contains additional information that an Expression in `expressions` may need.
/// For example, list of arguments expressions in `Expression.construct.components`
expressions_extra: std.ArrayListUnmanaged(Index(Expression)) = .{},
types: std.ArrayListUnmanaged(Type) = .{},

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    self.globals.deinit(allocator);
    self.expressions.deinit(allocator);
    self.expressions_extra.deinit(allocator);
    self.types.deinit(allocator);
}

pub const GlobalDecl = union(enum) {
    variable: GlobalVariable,
    function: Function,
    constant: Const,
    structure: Struct,
    type_alias: TypeAlias,
};

pub const GlobalVariable = struct {};

pub const Function = struct {};

pub const Const = struct {};

pub const Struct = struct {};

pub fn Index(comptime _: type) type {
    return usize;
}

pub fn Range(comptime _: type) type {
    return struct {
        start: usize,
        end: usize,
    };
}

pub const Literal = union(enum) {
    number: Number,
    bool: bool,
};

pub const Expression = union(enum) {
    literal: Literal,
    construct: struct {
        type: ConstructorType,
        components: Range(Index(Expression)),
    },
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
        arguments: []const Expression,
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

pub const UnaryOperator = enum {
    negate,
    not,
};

pub const BinaryOperator = enum {
    add,
    subtract,
    multiply,
    divide,
    modulo,
    equal,
    not_equal,
    less,
    less_equal,
    greater,
    greater_equal,
    @"and",
    exclusive_or,
    inclusive_or,
    logical_and,
    logical_or,
    shift_left,
    shift_right,
};

pub const ConstructorType = union(enum) {
    /// f32(1.0)
    scalar: ScalarType,

    /// vec3(1.0)
    partial_vector: PartialVectorType,

    /// vec3<f32>(1.0)
    vector: VectorType,

    /// mat2x2(1,2,3,4)
    partial_matrix: PartialMatrixType,

    /// mat2x2<f32>(1,2,3,4)
    matrix: MatrixType,

    /// array(1, 2, 3)
    partial_array,

    /// array<i32>(1, 2, 3)
    array: ArrayType,

    /// type my_vec = vec3<f32>;
    /// my_vec(1.0)
    user: []const u8,
};

pub const Number = union(enum) {
    /// Abstract Int (-2^63 ≤ i < 2^63)
    abstract_int: i64,
    /// Abstract Float (IEEE-754 binary64)
    abstract_float: f64,
    /// Concrete i32
    i32: i32,
    /// Concrete u32
    u32: u32,
    /// Concrete f32
    f32: f32,
    /// Concrete f16
    f16: f32,
};

pub const TypeAlias = struct {
    name: []const u8,
    type: Type,
};

pub const Type = union(enum) {
    scalar: ScalarType,
    atomic: AtomicType,
    vector: VectorType,
    matrix: MatrixType,
    array: ArrayType,
    sampler: SamplerType,
    /// A user-defined type, like a struct or a type alias.
    user: []const u8,
};

pub const ScalarType = struct {
    pub const Kind = enum {
        int,
        uint,
        float,
        bool,
    };

    kind: Kind,
    width: u8,
};

pub const AtomicType = struct {
    base: ScalarType,
};

pub const VectorType = struct {
    pub const Size = enum {
        bi,
        tri,
        quad,
    };

    size: Size,
    base: ScalarType,
};

pub const PartialVectorType = struct {
    size: VectorType.Size,
};

pub const MatrixType = struct {
    rows: VectorType.Size,
    columns: VectorType.Size,
    base: ScalarType,
};

pub const PartialMatrixType = struct {
    rows: VectorType.Size,
    columns: VectorType.Size,
};

pub const ArrayType = struct {
    pub const Size = union(enum) {
        constant: Index(Expression),
        dynamic,
    };
    base: Index(Type),
    size: Size,
};

pub const SamplerType = struct {
    comparison: bool,
};
