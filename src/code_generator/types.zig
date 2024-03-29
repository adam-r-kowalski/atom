const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const type_checker = @import("../type_checker.zig");
const Builtins = @import("../builtins.zig").Builtins;

pub const FunctionType = struct {
    parameters: []const Type,
    return_type: *const Type,
};

pub const Type = union(enum) {
    i32,
    i64,
    f32,
    f64,
    void,
    function: FunctionType,
};

pub const Parameter = struct {
    name: Interned,
    type: Type,
};

pub const BinaryOpKind = enum {
    i32_add,
    i32_sub,
    i32_mul,
    i32_div_s,
    i32_eq,
    i32_ge_u,
    i32_gt_s,
    i32_lt_s,
    i32_rem_s,
    i32_or,
    i32_store,
    i32_store8,
    i64_add,
    i64_sub,
    i64_mul,
    i64_div_s,
    i64_eq,
    i64_gt_s,
    i64_lt_s,
    i64_rem_s,
    i64_store,
    f32_add,
    f32_sub,
    f32_mul,
    f32_div,
    f32_eq,
    f32_gt,
    f32_lt,
    f32_store,
    f64_add,
    f64_sub,
    f64_mul,
    f64_div,
    f64_eq,
    f64_gt,
    f64_lt,
    f64_store,
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *const Expression,
    right: *const Expression,
};

const UnaryOpKind = enum {
    i32_trunc_f32_s,
    i64_trunc_f64_s,
    f32_sqrt,
    f32_convert_i32_s,
    f32_load,
    f64_sqrt,
    f64_convert_i64_s,
    f64_load,
    i32_load,
    i32_load8_u,
    i64_load,
};

pub const UnaryOp = struct {
    kind: UnaryOpKind,
    expression: *const Expression,
};

pub const Call = struct {
    function: Interned,
    arguments: []const Expression,
};

pub const Intrinsic = enum {
    empty,
};

pub const CallIntrinsic = struct {
    intrinsic: Intrinsic,
    arguments: []const Expression,
};

pub const If = struct {
    result: Type,
    condition: *const Expression,
    then: Expressions,
    else_: Expressions,
};

pub const LocalGet = struct {
    name: Interned,
};

pub const LocalSet = struct {
    name: Interned,
    value: *const Expression,
};

pub const GlobalGet = struct {
    name: Interned,
};

pub const GlobalSet = struct {
    name: Interned,
    value: *const Expression,
};

pub const Expressions = struct {
    expressions: []const Expression,
};

pub const Block = struct {
    result: Type,
    expressions: []const Expression,
};

pub const Literal = union(enum) {
    bool: bool,
    u32: u32,
    u64: u64,
    i32: i32,
    i64: i64,
    f32: Interned,
    f64: Interned,
};

pub const Drop = struct {
    expression: *const Expression,
};

pub const MemoryCopy = struct {
    destination: *const Expression,
    source: *const Expression,
    size: *const Expression,
};

pub const Expression = union(enum) {
    local_get: LocalGet,
    local_set: LocalSet,
    global_get: GlobalGet,
    global_set: GlobalSet,
    literal: Literal,
    call: Call,
    call_intrinsic: CallIntrinsic,
    if_: If,
    unary_op: UnaryOp,
    binary_op: BinaryOp,
    expressions: Expressions,
    block: Block,
    drop: Drop,
    memory_copy: MemoryCopy,
    nop,
    unreachable_,
};

pub const Local = struct {
    name: Interned,
    type: Type,
};

pub const LocalPointer = struct {
    name: Interned,
    size: u32,
};

pub const Function = struct {
    name: Interned,
    parameters: []const Parameter,
    return_type: Type,
    locals: []const Local,
    pointers: []const LocalPointer,
    body: Expressions,
};

pub const ForeignImport = struct {
    name: Interned,
    path: [2]Interned,
    type: Type,
};

pub const ForeignExport = struct {
    name: Interned,
    alias: Interned,
};

pub const Global = struct {
    name: Interned,
    type: Type,
    value: Expression,
};

pub const Offset = u32;

pub const Data = struct {
    offset: Offset,
    bytes: []const u8,
};

pub const DataSegment = struct {
    offset: Offset,
    data: List(Data),
};

pub const Intrinsics = std.AutoHashMap(Intrinsic, void);

pub const Module = struct {
    functions: []const Function,
    foreign_imports: []const ForeignImport,
    globals: []const Global,
    uses_memory: bool,
    data_segment: DataSegment,
    foreign_exports: []const ForeignExport,
    intrinsics: Intrinsics,
};
