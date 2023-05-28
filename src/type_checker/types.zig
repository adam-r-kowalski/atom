const std = @import("std");
const Allocator = std.mem.Allocator;
const Map = std.AutoHashMap;
const List = std.ArrayList;

const Builtins = @import("../builtins.zig").Builtins;
const interner = @import("../interner.zig");
const Interned = interner.Interned;
const parser = @import("../parser.zig");
const Span = parser.Span;
const BinaryOpKind = parser.BinaryOpKind;
const UntypedExpression = parser.Expression;

pub const TypeVar = u64;

pub const MonoType = union(enum) {
    void,
    i32,
    i64,
    f32,
    f64,
    bool,
    str,
    typevar: TypeVar,
    function: []const MonoType,
};

pub const Int = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Float = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Symbol = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Bool = struct {
    value: bool,
    span: Span,
    type: MonoType,
};

pub const String = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Define = struct {
    name: Symbol,
    value: *const Expression,
    span: Span,
    type: MonoType,
};

pub const Block = struct {
    expressions: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Function = struct {
    parameters: []const Symbol,
    return_type: MonoType,
    body: Block,
    span: Span,
    type: MonoType,
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *const Expression,
    right: *const Expression,
    span: Span,
    type: MonoType,
};

pub const If = struct {
    condition: *const Expression,
    then: Block,
    else_: Block,
    span: Span,
    type: MonoType,
};

pub const Cond = struct {
    conditions: []const Expression,
    thens: []const Block,
    else_: Block,
    span: Span,
    type: MonoType,
};

pub const Call = struct {
    function: *const Expression,
    arguments: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Intrinsic = struct {
    function: Interned,
    arguments: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Group = struct {
    expressions: []const Expression,
    span: Span,
    type: MonoType,
};

pub const ForeignImport = struct {
    module: Interned,
    name: Interned,
    span: Span,
    type: MonoType,
};

pub const Convert = struct {
    value: *const Expression,
    span: Span,
    type: MonoType,
};

pub const Expression = union(enum) {
    int: Int,
    float: Float,
    symbol: Symbol,
    bool: Bool,
    string: String,
    define: Define,
    function: Function,
    binary_op: BinaryOp,
    group: Group,
    block: Block,
    if_else: If,
    cond: Cond,
    call: Call,
    intrinsic: Intrinsic,
    foreign_import: ForeignImport,
    convert: Convert,
};

pub const Untyped = Map(Interned, UntypedExpression);
pub const Typed = Map(Interned, Expression);

fn expressionToMonoType(allocator: Allocator, builtins: Builtins, e: parser.Expression) !MonoType {
    switch (e) {
        .symbol => |s| {
            if (s.value == builtins.i32) return .i32;
            if (s.value == builtins.i64) return .i64;
            if (s.value == builtins.f32) return .f32;
            if (s.value == builtins.f64) return .f64;
            if (s.value == builtins.bool) return .bool;
            if (s.value == builtins.str) return .str;
            if (s.value == builtins.void) return .void;
            std.debug.panic("\nCannot convert symbol {} to mono type", .{s});
        },
        .prototype => |p| {
            const len = p.parameters.len;
            const function_type = try allocator.alloc(MonoType, len + 1);
            for (p.parameters, function_type[0..len]) |param, *t|
                t.* = try expressionToMonoType(allocator, builtins, param.type);
            function_type[len] = try expressionToMonoType(allocator, builtins, p.return_type.*);
            return MonoType{ .function = function_type };
        },
        else => std.debug.panic("\nCannot convert expression {} to mono type", .{e}),
    }
}

fn topLevelFunction(allocator: Allocator, builtins: Builtins, f: parser.Function) !MonoType {
    const len = f.parameters.len;
    const function_type = try allocator.alloc(MonoType, len + 1);
    for (f.parameters, function_type[0..len]) |p, *t|
        t.* = try expressionToMonoType(allocator, builtins, p.type);
    function_type[len] = try expressionToMonoType(allocator, builtins, f.return_type.*);
    return MonoType{ .function = function_type };
}

fn topLevelCall(allocator: Allocator, builtins: Builtins, c: parser.Call) !MonoType {
    switch (c.function.*) {
        .symbol => |s| {
            if (s.value == builtins.foreign_import) {
                if (c.arguments.len != 3) std.debug.panic("foreign_import takes 3 arguments", .{});
                return try expressionToMonoType(allocator, builtins, c.arguments[2]);
            }
        },
        else => |k| std.debug.panic("\nInvalid top level call function {}", .{k}),
    }
    std.debug.panic("\nInvalid top level call {}", .{c.function});
}

fn topLevelType(allocator: Allocator, builtins: Builtins, e: parser.Expression) !MonoType {
    return switch (e) {
        .function => |f| try topLevelFunction(allocator, builtins, f),
        .call => |c| try topLevelCall(allocator, builtins, c),
        else => |k| std.debug.panic("\nInvalid top level value {}", .{k}),
    };
}

pub const Ast = struct {
    order: []const Interned,
    untyped: Untyped,
    typed: Typed,
    scope: Scope,

    pub fn init(allocator: Allocator, builtins: Builtins, ast: parser.Ast) !Ast {
        var order = List(Interned).init(allocator);
        var untyped = Untyped.init(allocator);
        var typed = Typed.init(allocator);
        var scope = Scope.init(allocator);
        for (ast.expressions) |top_level| {
            switch (top_level) {
                .define => |d| {
                    const name = d.name.value;
                    try order.append(name);
                    try untyped.putNoClobber(name, top_level);
                    const monotype = try topLevelType(allocator, builtins, d.value.*);
                    try scope.put(name, monotype);
                },
                else => |k| std.debug.panic("\nInvalid top level expression {}", .{k}),
            }
        }
        return Ast{
            .order = try order.toOwnedSlice(),
            .untyped = untyped,
            .typed = typed,
            .scope = scope,
        };
    }
};

pub const Scope = Map(Interned, MonoType);

pub const Scopes = List(Scope);

pub const Substitution = Map(TypeVar, MonoType);

pub const Equal = struct {
    left: MonoType,
    right: MonoType,
};

pub const Constraints = struct {
    equal: List(Equal),
};
