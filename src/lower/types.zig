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

pub const Expression = union(enum) {
    i32_const: Interned,
    f32_const: Interned,
    i32_add: BinaryOp,
    f32_add: BinaryOp,
    block: []const Expression,
};

pub const Function = struct {
    name: Interned,
    parameters: []const Parameter,
    return_type: Type,
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
