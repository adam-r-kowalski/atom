const std = @import("std");
const Allocator = std.mem.Allocator;
const Map = std.AutoHashMap;
const List = std.ArrayList;

const Builtins = @import("../builtins.zig").Builtins;
const interner = @import("../interner.zig");
const Interned = interner.Interned;
const parser = @import("../parser.zig");
pub const Span = parser.types.Span;

pub const TypeVar = struct { value: u64 };

const ArrayMonoType = struct {
    size: ?u32,
    element_type: *const MonoType,
};

pub const MonoType = union(enum) {
    void,
    u8,
    i32,
    i64,
    f32,
    f64,
    bool,
    typevar: TypeVar,
    function: []MonoType,
    array: ArrayMonoType,
};

pub const Substitution = struct {
    map: Map(TypeVar, MonoType),
};

pub const TypedSpan = struct {
    span: ?Span,
    type: MonoType,
};

pub const EqualConstraint = struct {
    left: TypedSpan,
    right: TypedSpan,
};

pub const Constraints = struct {
    equal: List(EqualConstraint),
    next_type_var: TypeVar,
};

pub const Binding = struct {
    type: MonoType,
    global: bool,
    mutable: bool,
};

pub const Scope = Map(Interned, Binding);

pub const Int = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Float = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Symbol = struct {
    value: Interned,
    span: Span,
    type: MonoType,
    global: bool,
};

pub const Bool = struct {
    value: bool,
    span: Span,
    type: MonoType,
};

pub const String = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Define = struct {
    name: Symbol,
    value: *Expression,
    span: Span,
    mutable: bool,
    type: MonoType,
};

pub const Drop = struct {
    value: *Expression,
    span: Span,
    type: MonoType,
};

pub const PlusEqual = struct {
    name: Symbol,
    value: *Expression,
    span: Span,
    type: MonoType,
};

pub const TimesEqual = struct {
    name: Symbol,
    value: *Expression,
    span: Span,
    type: MonoType,
};

pub const Block = struct {
    expressions: []Expression,
    span: Span,
    type: MonoType,
};

pub const Parameter = struct {
    name: Symbol,
    mutable: bool,
};

pub const Function = struct {
    parameters: []Parameter,
    return_type: MonoType,
    body: Block,
    span: Span,
    type: MonoType,
};

pub const BinaryOp = struct {
    kind: parser.types.BinaryOpKind,
    left: *Expression,
    right: *Expression,
    span: Span,
    type: MonoType,
};

pub const Arm = struct {
    condition: Expression,
    then: Block,
};

pub const Branch = struct {
    arms: []Arm,
    else_: Block,
    span: Span,
    type: MonoType,
};

pub const Argument = struct {
    value: Expression,
    mutable: bool,
};

pub const Call = struct {
    function: *Expression,
    arguments: []Argument,
    span: Span,
    type: MonoType,
};

pub const Intrinsic = struct {
    function: Interned,
    arguments: []Expression,
    span: Span,
    type: MonoType,
};

pub const Group = struct {
    expressions: []Expression,
    span: Span,
    type: MonoType,
};

pub const ForeignImport = struct {
    module: Interned,
    name: Interned,
    span: Span,
    type: MonoType,
};

pub const ForeignExport = struct {
    name: Interned,
    value: *Expression,
    span: Span,
    type: MonoType,
};

pub const Convert = struct {
    value: *Expression,
    span: Span,
    type: MonoType,
};

pub const Undefined = struct {
    span: Span,
    type: MonoType,
};

pub const Expression = union(enum) {
    int: Int,
    float: Float,
    symbol: Symbol,
    bool: Bool,
    string: String,
    define: Define,
    drop: Drop,
    plus_equal: PlusEqual,
    times_equal: TimesEqual,
    function: Function,
    binary_op: BinaryOp,
    group: Group,
    block: Block,
    branch: Branch,
    call: Call,
    intrinsic: Intrinsic,
    foreign_import: ForeignImport,
    foreign_export: ForeignExport,
    convert: Convert,
    undefined: Undefined,
};

pub const Untyped = Map(Interned, parser.types.Expression);
pub const Typed = Map(Interned, Expression);

pub const Module = struct {
    allocator: Allocator,
    constraints: *Constraints,
    builtins: Builtins,
    order: []const Interned,
    untyped: Untyped,
    typed: Typed,
    scope: Scope,
    foreign_exports: []const Interned,
};
