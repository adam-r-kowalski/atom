const std = @import("std");
const Map = std.AutoHashMap;
const List = std.ArrayList;
const interner = @import("../interner.zig");
const Interned = interner.Interned;
const parser_types = @import("../parser/types.zig");
const Span = parser_types.Span;
const BinaryOpKind = parser_types.BinaryOpKind;
const UntypedExpression = parser_types.Expression;

pub const TypeVar = u64;

pub const MonoType = union(enum) {
    void,
    i32,
    i64,
    f32,
    f64,
    bool,
    str,
    typevar: TypeVar,
    function: []const MonoType,
};

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
    value: *const Expression,
    span: Span,
    type: MonoType,
};

pub const Block = struct {
    expressions: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Function = struct {
    parameters: []const Symbol,
    return_type: MonoType,
    body: Block,
    span: Span,
    type: MonoType,
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *const Expression,
    right: *const Expression,
    span: Span,
    type: MonoType,
};

pub const If = struct {
    condition: *const Expression,
    then: Block,
    else_: Block,
    span: Span,
    type: MonoType,
};

pub const Call = struct {
    function: *const Expression,
    arguments: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Intrinsic = struct {
    function: Interned,
    arguments: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Group = struct {
    expressions: []const Expression,
    span: Span,
    type: MonoType,
};

pub const ForeignImport = struct {
    module: Interned,
    name: Interned,
    span: Span,
    type: MonoType,
};

pub const Convert = struct {
    value: *const Expression,
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
    function: Function,
    binary_op: BinaryOp,
    group: Group,
    block: Block,
    if_: If,
    call: Call,
    intrinsic: Intrinsic,
    foreign_import: ForeignImport,
    convert: Convert,
};

pub const Untyped = Map(Interned, UntypedExpression);
pub const Typed = Map(Interned, Expression);

pub const Module = struct {
    order: []const Interned,
    untyped: Untyped,
    typed: Typed,
    scope: Scope,
};

pub const Scope = Map(Interned, MonoType);

pub const Scopes = List(Scope);

pub const Substitution = Map(TypeVar, MonoType);

pub const Equal = struct {
    left: MonoType,
    right: MonoType,
};

pub const Constraints = struct {
    equal: List(Equal),
};
