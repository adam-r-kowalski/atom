const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const tokenizer_types = @import("../tokenizer/types.zig");
pub const Span = tokenizer_types.Span;
pub const Int = tokenizer_types.Int;
pub const Symbol = tokenizer_types.Symbol;
pub const Bool = tokenizer_types.Bool;

pub const Define = struct {
    name: Symbol,
    type: ?*const Expression,
    body: []const Expression,
    span: Span,
};

pub const Parameter = struct {
    name: Symbol,
    type: ?Expression,
};

pub const Function = struct {
    name: Symbol,
    parameters: []const Parameter,
    return_type: ?*const Expression,
    body: []const Expression,
    span: Span,
};

pub const Declaration = struct {
    name: Symbol,
    parameters: []const Parameter,
    return_type: ?*const Expression,
    span: Span,
};

pub const BinaryOpKind = enum {
    add,
    subtract,
    multiply,
    exponentiate,
    greater,
    less,
    arrow,
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
    then: []const Expression,
    else_: []const Expression,
    span: Span,
};

pub const Call = struct {
    function: *const Expression,
    arguments: []const Expression,
    span: Span,
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
    function: Function,
    span: Span,
};

pub const Export = struct {
    function: Function,
    span: Span,
};

pub const TopLevel = union(enum) {
    import: Import,
    export_: Export,
    define: Define,
    function: Function,
};

pub const Module = struct {
    top_level: []const TopLevel,
    span: Span,
};
