const tokenizer = @import("../tokenizer.zig");

pub const Span = tokenizer.types.Span;
pub const Int = tokenizer.types.Int;
pub const Float = tokenizer.types.Float;
pub const Symbol = tokenizer.types.Symbol;
pub const String = tokenizer.types.String;
pub const Bool = tokenizer.types.Bool;
pub const Undefined = tokenizer.types.Undefined;

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

pub const Function = struct {
    parameters: []const Parameter,
    return_type: *const Expression,
    body: Block,
    span: Span,
};

pub const Prototype = struct {
    parameters: []const Parameter,
    return_type: *const Expression,
    span: Span,
};

pub const Enumeration = struct {
    variants: []const Symbol,
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
    dot,
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *const Expression,
    right: *const Expression,
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

pub const Call = struct {
    function: *const Expression,
    arguments: []const Argument,
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
    prototype: Prototype,
    binary_op: BinaryOp,
    group: Group,
    block: Block,
    array: Array,
    branch: Branch,
    call: Call,
    undefined: Undefined,
};

pub const TopLevelEnumeration = struct {
    name: Symbol,
    type: ?*const Expression,
    enumeration: Enumeration,
    span: Span,
};

pub const TopLevelFunction = struct {
    name: Symbol,
    type: ?*const Expression,
    function: Function,
    span: Span,
};

pub const TopLevelForeignImport = struct {
    name: Symbol,
    type: ?*const Expression,
    call: Call,
    span: Span,
};

pub const Module = struct {
    foreign_imports: []const TopLevelForeignImport,
    enumerations: []const TopLevelEnumeration,
    functions: []const TopLevelFunction,
    foreign_exports: []const Call,
    ignored: []const Expression,
};
