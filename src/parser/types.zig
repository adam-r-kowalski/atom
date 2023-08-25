const std = @import("std");
const Map = std.AutoHashMap;

const tokenizer = @import("../tokenizer.zig");

pub const Span = tokenizer.types.Span;
pub const Pos = tokenizer.types.Pos;
pub const Int = tokenizer.types.Int;
pub const Float = tokenizer.types.Float;
pub const Symbol = tokenizer.types.Symbol;
pub const String = tokenizer.types.String;
pub const Bool = tokenizer.types.Bool;
pub const Undefined = tokenizer.types.Undefined;
pub const Attribute = tokenizer.types.Attribute;

const Interned = @import("../interner.zig").Interned;

pub const Define = struct {
    name: Symbol,
    type: ?*const Expression,
    value: *const Expression,
    mutable: bool,
    span: Span,
};

pub const Drop = struct {
    type: ?*const Expression,
    value: *const Expression,
    span: Span,
};

pub const PlusEqual = struct {
    name: Symbol,
    value: *const Expression,
    span: Span,
};

pub const TimesEqual = struct {
    name: Symbol,
    value: *const Expression,
    span: Span,
};

pub const Parameter = struct {
    name: Symbol,
    type: Expression,
    mutable: bool,
    span: Span,
};

pub const Block = struct {
    expressions: []const Expression,
    span: Span,
};

pub const Array = struct {
    expressions: []const Expression,
    span: Span,
};

pub const ArrayType = struct {
    of: *const Expression,
    span: Span,
};

pub const Function = struct {
    name: Symbol,
    parameters: []const Parameter,
    return_type: *const Expression,
    body: Block,
    span: Span,
};

pub const Prototype = struct {
    name: Symbol,
    parameters: []const Parameter,
    return_type: *const Expression,
    span: Span,
};

pub const Enumeration = struct {
    name: Symbol,
    variants: []const Symbol,
    span: Span,
};

pub const StructField = struct {
    name: Symbol,
    type: Expression,
    span: Span,
};

pub const StructFields = Map(Interned, StructField);

pub const Structure = struct {
    name: Symbol,
    fields: StructFields,
    order: []const Interned,
    span: Span,
};

pub const StructLiteralField = struct {
    name: Symbol,
    value: Expression,
    span: Span,
};

pub const StructLiteralFields = Map(Interned, StructLiteralField);

pub const StructLiteral = struct {
    fields: StructLiteralFields,
    order: []const Interned,
    span: Span,
};

pub const BinaryOpKind = enum {
    add,
    subtract,
    multiply,
    divide,
    modulo,
    exponentiate,
    equal,
    greater,
    less,
    or_,
    pipeline,
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *const Expression,
    right: *const Expression,
    span: Span,
};

pub const Dot = struct {
    left: *const Expression,
    right: Symbol,
    span: Span,
};

pub const Group = struct {
    expression: *const Expression,
    span: Span,
};

pub const Arm = struct {
    condition: Expression,
    then: Block,
};

pub const Branch = struct {
    arms: []const Arm,
    else_: Block,
    span: Span,
};

pub const Argument = struct {
    value: Expression,
    mutable: bool,
    span: Span,
};

pub const Arguments = struct {
    positional: []const Argument,
    named: Map(Interned, Argument),
    named_order: []const Interned,
    span: Span,
};

pub const Call = struct {
    function: *const Expression,
    arguments: Arguments,
    span: Span,
};

pub const Decorator = struct {
    attribute: Attribute,
    arguments: ?Arguments,
    value: *const Expression,
    span: Span,
};

pub const Index = struct {
    expression: *const Expression,
    indices: []const Expression,
    span: Span,
};

pub const TemplateLiteral = struct {
    function: ?Symbol,
    strings: []const String,
    arguments: []const Expression,
    span: Span,
};

pub const Expression = union(enum) {
    int: Int,
    float: Float,
    symbol: Symbol,
    string: String,
    bool: Bool,
    define: Define,
    drop: Drop,
    plus_equal: PlusEqual,
    times_equal: TimesEqual,
    function: Function,
    enumeration: Enumeration,
    structure: Structure,
    prototype: Prototype,
    binary_op: BinaryOp,
    dot: Dot,
    group: Group,
    block: Block,
    array: Array,
    array_type: ArrayType,
    branch: Branch,
    call: Call,
    decorator: Decorator,
    index: Index,
    template_literal: TemplateLiteral,
    undefined: Undefined,
};

pub const Module = struct {
    foreign_imports: []const Decorator,
    structures: []const Structure,
    enumerations: []const Enumeration,
    functions: []const Function,
    defines: []const Define,
    foreign_exports: []const Decorator,
    ignored: []const Expression,
};
