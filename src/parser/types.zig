const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const tokenizer_types = @import("../tokenizer/types.zig");
const Token = tokenizer_types.Token;
const Span = tokenizer_types.Span;
const Indent = tokenizer_types.Indent;
pub const Int = tokenizer_types.Int;
pub const Symbol = tokenizer_types.Symbol;
pub const Bool = tokenizer_types.Bool;

pub const Define = struct {
    name: *const Ast,
    type: ?*const Ast,
    body: []const Ast,
    span: Span,
};

pub const Parameter = struct {
    name: Ast,
    type: ?Ast,
};

pub const Function = struct {
    name: *const Ast,
    parameters: []const Parameter,
    return_type: ?*const Ast,
    body: []const Ast,
    span: Span,
};

pub const Declaration = struct {
    name: *const Ast,
    parameters: []const Parameter,
    return_type: ?*const Ast,
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
    left: *const Ast,
    right: *const Ast,
    span: Span,
};

pub const Group = struct {
    expression: *const Ast,
    span: Span,
};

pub const If = struct {
    condition: *const Ast,
    then: []const Ast,
    else_: []const Ast,
    span: Span,
};

pub const Call = struct {
    function: *const Ast,
    arguments: []const Ast,
    span: Span,
};

pub const Import = struct {
    expression: *const Ast,
    span: Span,
};

pub const Export = struct {
    expression: *const Ast,
    span: Span,
};

pub const Module = struct {
    expressions: []const Ast,
    span: Span,
};

pub const Ast = union(enum) {
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
    import: Import,
    export_: Export,
    module: Module,
};
