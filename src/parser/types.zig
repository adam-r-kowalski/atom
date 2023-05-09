const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const tokenizer_types = @import("../tokenizer/types.zig");
const Token = tokenizer_types.Token;
const Span = tokenizer_types.Span;
const Indent = tokenizer_types.Indent;

pub const Define = struct {
    name: *const Ast,
    type: ?*const Ast,
    body: []const Ast,
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
};

pub const Declaration = struct {
    name: *const Ast,
    parameters: []const Parameter,
    return_type: ?*const Ast,
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
};

pub const If = struct {
    condition: *const Ast,
    then: []const Ast,
    else_: []const Ast,
};

pub const Call = struct {
    function: *const Ast,
    arguments: []const Ast,
};

pub const Module = []const Ast;

pub const Kind = union(enum) {
    int: Interned,
    symbol: Interned,
    define: Define,
    function: Function,
    declaration: Declaration,
    binary_op: BinaryOp,
    group: *const Ast,
    if_: If,
    call: Call,
    bool: bool,
    import: *const Ast,
    export_: *const Ast,
    module: Module,
};

pub const Ast = struct {
    kind: Kind,
    span: Span,
};
