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

pub const Expression = union(enum) {
    i32: Interned,
    f32: Interned,
};

pub const Function = struct {
    name: Interned,
    parameters: []const Parameter,
    return_type: Type,
    body: []const Expression,
};

pub const IR = struct {
    functions: []const Function,
};
