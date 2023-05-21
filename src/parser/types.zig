const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const tokenizer_types = @import("../tokenizer/types.zig");
pub const Span = tokenizer_types.Span;

pub const Define = struct {
    name: *const Expression,
    type: ?*const Expression,
    value: *const Expression,
};

pub const Parameter = struct {
    name: *const Expression,
    type: *const Expression,
};

pub const Function = struct {
    parameters: []const Parameter,
    return_type: *const Expression,
    body: *const Expression,
};

pub const BinaryOpKind = enum {
    add,
    subtract,
    multiply,
    exponentiate,
    greater,
    less,
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
};

pub const Module = struct {
    expressions: []const Expression,
};
