const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const tokenizer_types = @import("../tokenizer/types.zig");
pub const Span = tokenizer_types.Span;
pub const Int = tokenizer_types.Int;
pub const Float = tokenizer_types.Float;
pub const Symbol = tokenizer_types.Symbol;
pub const String = tokenizer_types.String;
pub const Bool = tokenizer_types.Bool;

pub const Define = struct {
    name: Symbol,
    type: ?*const Expression,
    value: *const Expression,
    span: Span,
};

pub const Parameter = struct {
    name: Symbol,
    type: Expression,
};

pub const Block = struct {
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

pub const If = struct {
    condition: *const Expression,
    then: Block,
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
    function: Function,
    prototype: Prototype,
    binary_op: BinaryOp,
    group: Group,
    block: Block,
    if_: If,
    call: Call,
};

pub const Module = struct {
    expressions: []const Expression,
};
