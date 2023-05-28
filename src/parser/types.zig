const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const tokenizer = @import("../tokenizer.zig");
pub const Span = tokenizer.Span;
pub const Int = tokenizer.Int;
pub const Float = tokenizer.Float;
pub const Symbol = tokenizer.Symbol;
pub const String = tokenizer.String;
pub const Bool = tokenizer.Bool;

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

pub const If = struct {
    condition: *const Expression,
    then: Block,
    else_: Block,
    span: Span,
};

pub const Cond = struct {
    conditions: []const Expression,
    thens: []const Block,
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
    if_else: If,
    cond: Cond,
    call: Call,

    pub fn span(self: Expression) Span {
        return switch (self) {
            .int => |e| e.span,
            .float => |e| e.span,
            .symbol => |e| e.span,
            .string => |e| e.span,
            .bool => |e| e.span,
            .define => |e| e.span,
            .function => |e| e.span,
            .prototype => |e| e.span,
            .binary_op => |e| e.span,
            .group => |e| e.span,
            .block => |e| e.span,
            .if_else => |e| e.span,
            .cond => |e| e.span,
            .call => |e| e.span,
        };
    }
};

pub const Module = struct {
    expressions: []const Expression,
};
