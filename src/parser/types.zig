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
    mut: bool,
};

pub const Block = struct {
    expressions: []const Expression,
    span: Span,
};

pub const Array = struct {
    expressions: []const Expression,
    span: Span,
};

pub const ArrayOf = struct {
    size: ?Int,
    element_type: *const Expression,
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

pub const Call = struct {
    function: *const Expression,
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
    plus_equal: PlusEqual,
    times_equal: TimesEqual,
    function: Function,
    prototype: Prototype,
    binary_op: BinaryOp,
    group: Group,
    block: Block,
    array: Array,
    array_of: ArrayOf,
    branch: Branch,
    call: Call,
    undefined: Undefined,
};

pub const Module = struct { expressions: []const Expression };
