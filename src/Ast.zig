const std = @import("std");
const Token = @import("Token.zig");

const Ast = @This();

globals: std.ArrayListUnmanaged(GlobalDecl),

/// contains references used in base field in `ArrayType`
/// this exist because of dependency-loop
array_bases: std.SegmentedList(Type, 1024), // TODO: think more about prealloc size

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    self.globals.deinit(allocator);
    self.array_bases.deinit(allocator);
}

pub const GlobalDecl = struct {
    kind: Kind,

    pub const Kind = union(enum) {
        variable: GlobalVariable,
        function: Function,
        constant: Const,
        structure: Struct,
        type_alias: TypeAlias,
    };
};

pub const GlobalVariable = struct {};

pub const Function = struct {};

pub const Const = struct {};

pub const Struct = struct {};

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
    binding_array: ArrayType,
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
    kind: ScalarType.Kind,
    width: u8,
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

pub const MatrixType = struct {
    rows: VectorType.Size,
    columns: VectorType.Size,
    base: ScalarType,
};

pub const ArrayType = struct {
    pub const Size = union(enum) {
        constant: Number, // TODO: make this an Expression
        dynamic,
    };
    base: *const Type,
    size: Size,
};

pub const SamplerType = struct {
    comparison: bool,
};
