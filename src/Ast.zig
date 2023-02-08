const std = @import("std");
const Token = @import("Token.zig");

const Ast = @This();

/// TODO: make these MultiArrayList
/// Entry of the parse result
globals: std.ArrayListUnmanaged(GlobalDecl) = .{},
/// Contains expression's that a GlobalDecl in `globals` may need
expressions: std.ArrayListUnmanaged(Expression) = .{},
/// Contains PlainType's that an ArrayType `element_type` field need
types: std.ArrayListUnmanaged(PlainType) = .{},

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    self.globals.deinit(allocator);
    self.expressions.deinit(allocator);
    self.types.deinit(allocator);
}

pub fn Span(comptime T: type) type {
    return union(enum) {
        zero,
        one: Index(T),
        multi: Range(T),
    };
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
    variable: GlobalVariable,
    function: Function,
    constant: Const,
    strct: Struct,
    type_alias: TypeAlias,
};

pub const GlobalVariable = struct {};

pub const Function = struct {};

pub const Const = struct {};

pub const Struct = struct {};

pub const TypeAlias = struct {
    name: []const u8,
    type: PlainType,
};

pub const Expression = union(enum) {
    literal: Literal,
    construct: ConstructExpr,
    unary_operator: UnaryOperator,
    addr_of: Index(Expression),
    deref: Index(Expression),
    binary: BinaryOperator,
    call: FunctionCall,
    index: struct { // TODO
        base: Index(Expression),
        index: Index(Expression),
    },
    member: struct { // TODO
        base: Index(Expression),
        field: []const u8,
    },
    bitcast: BitcastExpr,
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

pub const ConstructExpr = struct {
    type: union(enum) {
        /// f32(e1,...eN), ...
        scalar: ScalarType,
        /// vec3<T?>(e1,...eN)
        vector: VectorType,
        /// mat2x2<T?>(e1,...eN)
        matrix: MatrixType,
        /// array(e1,...eN)
        partial_array,
        /// array<T, N?>(e1,...eN)
        full_array: ArrayType,
        user: []const u8,
    },
    components: Span(Expression),
};

pub const BitcastExpr = struct {
    /// only i32, u32 and f32 is allowed
    dest: ScalarType,
    expr: Index(Expression),
};

pub const FunctionCall = struct {
    function: []const u8,
    arguments: Range(Expression),
};

pub const UnaryOperator = struct {
    pub const Kind = enum {
        negate,
        not,
    };

    op: Kind,
    expr: Index(Expression),
};

pub const BinaryOperator = struct {
    pub const Kind = enum {
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

    op: Kind,
    left: Index(Expression),
    right: Index(Expression),
};

pub const PlainType = union(enum) {
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

pub const VectorType = union(enum) {
    partial: Partial,
    full: Full,

    pub const Full = struct {
        size: Size,
        element_type: ScalarType,
    };

    pub const Partial = struct {
        size: Size,
    };

    pub const Size = enum {
        bi,
        tri,
        quad,
    };
};

pub const MatrixType = union(enum) {
    partial: Partial,
    full: Full,

    pub const Full = struct {
        rows: VectorType.Size,
        columns: VectorType.Size,
        element_type: ScalarType,
    };

    pub const Partial = struct {
        rows: VectorType.Size,
        columns: VectorType.Size,
    };
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
    element_type: Index(PlainType),
};
