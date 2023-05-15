const std = @import("std");
const Map = std.AutoHashMap;
const List = std.ArrayList;
const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const parser_types = @import("../parser/types.zig");
const Span = parser_types.Span;
const BinaryOpKind = parser_types.BinaryOpKind;
const UntypedTopLevel = parser_types.TopLevel;

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

pub const Symbol = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Int = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Bool = struct {
    value: bool,
    span: Span,
    type: MonoType,
};

pub const Define = struct {
    name: Symbol,
    body: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Function = struct {
    name: Symbol,
    parameters: []const Symbol,
    return_type: MonoType,
    body: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Declaration = struct {
    name: Symbol,
    parameters: []const Symbol,
    return_type: MonoType,
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

pub const Group = struct {
    expression: *const Expression,
    span: Span,
    type: MonoType,
};

pub const If = struct {
    condition: *const Expression,
    then: []const Expression,
    else_: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Call = struct {
    function: *const Expression,
    arguments: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Expression = union(enum) {
    int: Int,
    symbol: Symbol,
    define: Define,
    function: Function,
    declaration: Declaration,
    binary_op: BinaryOp,
    group: Group,
    if_: If,
    call: Call,
    bool: Bool,
};

pub const Import = struct {
    expression: Expression,
    span: Span,
    type: MonoType,
};

pub const Export = struct {
    expression: Expression,
    span: Span,
    type: MonoType,
};

pub const TopLevel = union(enum) {
    import: Import,
    export_: Export,
    define: Define,
    function: Function,
};

pub const Untyped = Map(Interned, UntypedTopLevel);
pub const Typed = Map(Interned, TopLevel);

pub const Module = struct {
    order: []const Interned,
    untyped: Untyped,
    typed: Typed,
    scope: Scope,
    span: Span,
    type: MonoType,
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
