const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const tokenizer_types = @import("../tokenizer/types.zig");
const Token = tokenizer_types.Token;
const Span = tokenizer_types.Span;
const Indent = tokenizer_types.Indent;

pub const MonoType = union(enum) {
    i32,
    bool,
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
    body: []const Ast,
    span: Span,
    type: MonoType,
};

pub const Function = struct {
    name: Symbol,
    parameters: []const Symbol,
    return_type: MonoType,
    body: []const Ast,
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
    type: MonoType,
};

pub const Group = struct {
    expression: *const Ast,
    span: Span,
    type: MonoType,
};

pub const If = struct {
    condition: *const Ast,
    then: []const Ast,
    else_: []const Ast,
    span: Span,
    type: MonoType,
};

pub const Call = struct {
    function: *const Ast,
    arguments: []const Ast,
    span: Span,
    type: MonoType,
};

pub const Import = struct {
    expression: *const Ast,
    span: Span,
    type: MonoType,
};

pub const Export = struct {
    expression: *const Ast,
    span: Span,
    type: MonoType,
};

pub const Module = struct {
    expressions: []const Ast,
    span: Span,
    type: MonoType,
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
