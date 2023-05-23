const interner = @import("../interner.zig");
const Interned = interner.Interned;

pub const Type = enum {
    i32,
    f32,
};

pub const Parameter = struct {
    name: Interned,
    type: Type,
};

pub const BinaryOp = struct {
    left: *const Expression,
    right: *const Expression,
};

pub const Call = struct {
    function: Interned,
    arguments: []const Expression,
};

pub const If = struct {
    result: Type,
    condition: *const Expression,
    then: *const Expression,
    else_: *const Expression,
};

pub const LocalSet = struct {
    name: Interned,
    value: *const Expression,
};

pub const Expression = union(enum) {
    local_get: Interned,
    local_set: LocalSet,
    i32_const: Interned,
    i32_add: BinaryOp,
    i32_sub: BinaryOp,
    i32_mul: BinaryOp,
    i32_eq: BinaryOp,
    i32_gt_s: BinaryOp,
    i32_rem_s: BinaryOp,
    i32_or: BinaryOp,
    f32_const: Interned,
    f32_add: BinaryOp,
    f32_sub: BinaryOp,
    f32_mul: BinaryOp,
    f32_eq: BinaryOp,
    f32_gt: BinaryOp,
    block: []const Expression,
    call: Call,
    if_: If,
};

pub const Local = struct {
    name: Interned,
    type: Type,
};

pub const Function = struct {
    name: Interned,
    parameters: []const Parameter,
    return_type: Type,
    locals: []const Local,
    body: *const Expression,
};

pub const FunctionExport = struct {
    name: Interned,
    alias: Interned,
};

pub const Export = union(enum) {
    function: FunctionExport,
};

pub const IR = struct {
    functions: []const Function,
    exports: []const Export,
};
