const interner = @import("../interner.zig");
const Interned = interner.Interned;

pub const Type = union(enum) {
    i32,
    i64,
    f32,
    void,
    function: []const Type,
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
    then: []const Expression,
    else_: []const Expression,
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
    i32_div_s: BinaryOp,
    i32_eq: BinaryOp,
    i32_gt_s: BinaryOp,
    i32_rem_s: BinaryOp,
    i32_or: BinaryOp,
    i32_trunc_f32_s: *const Expression,
    i64_const: Interned,
    i64_add: BinaryOp,
    i64_sub: BinaryOp,
    i64_mul: BinaryOp,
    i64_div_s: BinaryOp,
    i64_eq: BinaryOp,
    i64_gt_s: BinaryOp,
    i64_rem_s: BinaryOp,
    f32_const: Interned,
    f32_add: BinaryOp,
    f32_sub: BinaryOp,
    f32_mul: BinaryOp,
    f32_div: BinaryOp,
    f32_eq: BinaryOp,
    f32_gt: BinaryOp,
    f32_sqrt: *const Expression,
    f32_convert_i32_s: *const Expression,
    call: Call,
    if_: If,
    block: []const Expression,
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
    body: []const Expression,
};

pub const Import = struct {
    name: Interned,
    path: [2]Interned,
    type: Type,
};

pub const Export = struct {
    name: Interned,
    alias: Interned,
};

pub const IR = struct {
    functions: []const Function,
    imports: []const Import,
    exports: []const Export,
};
