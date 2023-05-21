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
    f32,
    bool,
    module,
    typevar: TypeVar,
    function: []const MonoType,
};

pub const Define = struct {
    name: *const Expression,
    value: *const Expression,
};

pub const Function = struct {
    parameters: []const Expression,
    return_type: MonoType,
    body: *const Expression,
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *const Expression,
    right: *const Expression,
};

pub const If = struct {
    condition: *const Expression,
    then: *const Expression,
    else_: *const Expression,
};

pub const Call = struct {
    function: *const Expression,
    arguments: []const Expression,
};

pub const Kind = union(enum) {
    int: Interned,
    float: Interned,
    symbol: Interned,
    bool: bool,
    define: Define,
    function: Function,
    binary_op: BinaryOp,
    group: *const Expression,
    block: []const Expression,
    if_: If,
    call: Call,
};

pub const Expression = struct {
    kind: Kind,
    span: Span,
    type: MonoType,
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
