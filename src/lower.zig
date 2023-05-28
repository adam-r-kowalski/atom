const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const type_checker = @import("type_checker.zig");
const Ast = type_checker.Ast;
const MonoType = type_checker.MonoType;
const Builtins = @import("builtins.zig").Builtins;

pub const Type = union(enum) {
    i32,
    i64,
    f32,
    f64,
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
    i32_lt_s: BinaryOp,
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
    i64_lt_s: BinaryOp,
    i64_rem_s: BinaryOp,
    i64_trunc_f64_s: *const Expression,
    f32_const: Interned,
    f32_add: BinaryOp,
    f32_sub: BinaryOp,
    f32_mul: BinaryOp,
    f32_div: BinaryOp,
    f32_eq: BinaryOp,
    f32_gt: BinaryOp,
    f32_lt: BinaryOp,
    f32_sqrt: *const Expression,
    f32_convert_i32_s: *const Expression,
    f64_const: Interned,
    f64_add: BinaryOp,
    f64_sub: BinaryOp,
    f64_mul: BinaryOp,
    f64_div: BinaryOp,
    f64_eq: BinaryOp,
    f64_gt: BinaryOp,
    f64_lt: BinaryOp,
    f64_sqrt: *const Expression,
    f64_convert_i64_s: *const Expression,
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

fn mapType(monotype: MonoType) Type {
    switch (monotype) {
        .i32 => return .i32,
        .i64 => return .i64,
        .f32 => return .f32,
        .f64 => return .f64,
        .bool => return .i32,
        .void => return .void,
        else => std.debug.panic("\nMonotype {} not yet supported", .{monotype}),
    }
}

fn int(i: type_checker.Int) !Expression {
    switch (i.type) {
        .i32 => return .{ .i32_const = i.value },
        .i64 => return .{ .i64_const = i.value },
        .f32 => return .{ .f32_const = i.value },
        .f64 => return .{ .f64_const = i.value },
        else => |k| std.debug.panic("\nInt type {} not yet supported", .{k}),
    }
}

fn float(f: type_checker.Float) !Expression {
    switch (f.type) {
        .f32 => return .{ .f32_const = f.value },
        else => |k| std.debug.panic("\nFloat type {} not yet supported", .{k}),
    }
}

fn boolean(builtins: Builtins, b: type_checker.Bool) !Expression {
    return Expression{
        .i32_const = if (b.value) builtins.one else builtins.zero,
    };
}

fn block(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker.Block) ![]const Expression {
    const expressions = try allocator.alloc(Expression, b.expressions.len);
    for (b.expressions, expressions) |expr, *ir_expr| {
        ir_expr.* = try expression(allocator, builtins, locals, expr);
    }
    return expressions;
}

fn add(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .i32_add = .{ .left = left, .right = right } },
        .i64 => return Expression{ .i64_add = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_add = .{ .left = left, .right = right } },
        .f64 => return Expression{ .f64_add = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nAdd type {} not yet supported", .{k}),
    }
}

fn subtract(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .i32_sub = .{ .left = left, .right = right } },
        .i64 => return Expression{ .i64_sub = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_sub = .{ .left = left, .right = right } },
        .f64 => return Expression{ .f64_sub = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nSubtract type {} not yet supported", .{k}),
    }
}

fn multiply(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .i32_mul = .{ .left = left, .right = right } },
        .i64 => return Expression{ .i64_mul = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_mul = .{ .left = left, .right = right } },
        .f64 => return Expression{ .f64_mul = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nMultiply type {} not yet supported", .{k}),
    }
}

fn divide(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .i32_div_s = .{ .left = left, .right = right } },
        .i64 => return Expression{ .i64_div_s = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_div = .{ .left = left, .right = right } },
        .f64 => return Expression{ .f64_div = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nDivide type {} not yet supported", .{k}),
    }
}

fn modulo(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .i32_rem_s = .{ .left = left, .right = right } },
        .i64 => return Expression{ .i64_rem_s = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nModulo type {} not yet supported", .{k}),
    }
}

fn equal(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .i32_eq = .{ .left = left, .right = right } },
        .i64 => return Expression{ .i64_eq = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_eq = .{ .left = left, .right = right } },
        .f64 => return Expression{ .f64_eq = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nEqual type {} not yet supported", .{k}),
    }
}

fn binaryOr(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.typeOf()) {
        .bool => return Expression{ .i32_or = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nOr type {} not yet supported", .{k}),
    }
}

fn greater(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .i32_gt_s = .{ .left = left, .right = right } },
        .i64 => return Expression{ .i64_gt_s = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_gt = .{ .left = left, .right = right } },
        .f64 => return Expression{ .f64_gt = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nGreater type {} not yet supported", .{k}),
    }
}

fn less(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.typeOf()) {
        .i32 => return Expression{ .i32_lt_s = .{ .left = left, .right = right } },
        .i64 => return Expression{ .i64_lt_s = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_lt = .{ .left = left, .right = right } },
        .f64 => return Expression{ .f64_lt = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nGreater type {} not yet supported", .{k}),
    }
}

fn binaryOp(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker.BinaryOp) !Expression {
    switch (b.kind) {
        .add => return try add(allocator, builtins, locals, b),
        .subtract => return try subtract(allocator, builtins, locals, b),
        .multiply => return try multiply(allocator, builtins, locals, b),
        .divide => return try divide(allocator, builtins, locals, b),
        .modulo => return try modulo(allocator, builtins, locals, b),
        .equal => return try equal(allocator, builtins, locals, b),
        .or_ => return try binaryOr(allocator, builtins, locals, b),
        .greater => return try greater(allocator, builtins, locals, b),
        .less => return try less(allocator, builtins, locals, b),
        else => |k| std.debug.panic("\nBinary op {} not yet supported", .{k}),
    }
}

fn symbol(s: type_checker.Symbol) Expression {
    return Expression{ .local_get = s.value };
}

fn call(allocator: Allocator, builtins: Builtins, locals: *List(Local), c: type_checker.Call) !Expression {
    switch (c.function.*) {
        .symbol => |s| {
            const arguments = try allocator.alloc(Expression, c.arguments.len);
            for (c.arguments, arguments) |arg, *ir_arg| {
                ir_arg.* = try expression(allocator, builtins, locals, arg);
            }
            return Expression{
                .call = .{
                    .function = s.value,
                    .arguments = arguments,
                },
            };
        },
        else => |k| std.debug.panic("\nCall function type {} not yet supported", .{k}),
    }
}

fn intrinsic(allocator: Allocator, builtins: Builtins, locals: *List(Local), i: type_checker.Intrinsic) !Expression {
    if (i.function == builtins.sqrt) {
        switch (i.type) {
            .f32 => return Expression{ .f32_sqrt = try expressionAlloc(allocator, builtins, locals, i.arguments[0]) },
            .f64 => return Expression{ .f64_sqrt = try expressionAlloc(allocator, builtins, locals, i.arguments[0]) },
            else => |k| std.debug.panic("\nSqrt type {} not yet supported", .{k}),
        }
    }
    std.debug.panic("\nIntrinsic {} not yet supported", .{i.function});
}

fn ifElse(allocator: Allocator, builtins: Builtins, locals: *List(Local), i: type_checker.If) !Expression {
    const condition = try expressionAlloc(allocator, builtins, locals, i.condition.*);
    const then = try block(allocator, builtins, locals, i.then);
    const else_ = try block(allocator, builtins, locals, i.else_);
    return Expression{
        .if_ = .{
            .result = mapType(i.type),
            .condition = condition,
            .then = then,
            .else_ = else_,
        },
    };
}

fn cond(allocator: Allocator, builtins: Builtins, locals: *List(Local), c: type_checker.Cond) !Expression {
    const len = c.conditions.len;
    var result = Expression{
        .if_ = .{
            .result = mapType(c.type),
            .condition = try expressionAlloc(allocator, builtins, locals, c.conditions[len - 1]),
            .then = try block(allocator, builtins, locals, c.thens[len - 1]),
            .else_ = try block(allocator, builtins, locals, c.else_),
        },
    };
    var i: usize = len - 1;
    while (i > 0) : (i -= 1) {
        const else_ = try allocator.alloc(Expression, 1);
        else_[0] = result;
        result = Expression{
            .if_ = .{
                .result = mapType(c.type),
                .condition = try expressionAlloc(allocator, builtins, locals, c.conditions[i - 1]),
                .then = try block(allocator, builtins, locals, c.thens[i - 1]),
                .else_ = else_,
            },
        };
    }
    return result;
}

fn define(allocator: Allocator, builtins: Builtins, locals: *List(Local), d: type_checker.Define) !Expression {
    const name = d.name.value;
    const value = try expressionAlloc(allocator, builtins, locals, d.value.*);
    try locals.append(Local{ .name = name, .type = mapType(d.name.type) });
    return Expression{ .local_set = .{ .name = name, .value = value } };
}

fn convert(allocator: Allocator, builtins: Builtins, locals: *List(Local), c: type_checker.Convert) !Expression {
    const value = try expressionAlloc(allocator, builtins, locals, c.value.*);
    switch (c.value.typeOf()) {
        .i32 => switch (c.type) {
            .f32 => return Expression{ .f32_convert_i32_s = value },
            else => |k| std.debug.panic("\nConvert type i32 to {} not yet supported", .{k}),
        },
        .f32 => switch (c.type) {
            .i32 => return Expression{ .i32_trunc_f32_s = value },
            else => |k| std.debug.panic("\nConvert type f32 to {} not yet supported", .{k}),
        },
        .i64 => switch (c.type) {
            .f64 => return Expression{ .f64_convert_i64_s = value },
            else => |k| std.debug.panic("\nConvert type i64 to {} not yet supported", .{k}),
        },
        .f64 => switch (c.type) {
            .i64 => return Expression{ .i64_trunc_f64_s = value },
            else => |k| std.debug.panic("\nConvert type f64 to {} not yet supported", .{k}),
        },
        else => |k| std.debug.panic("\nConvert type {} not yet supported", .{k}),
    }
}

fn expression(allocator: Allocator, builtins: Builtins, locals: *List(Local), e: type_checker.Expression) error{OutOfMemory}!Expression {
    switch (e) {
        .int => |i| return try int(i),
        .float => |f| return try float(f),
        .bool => |b| return try boolean(builtins, b),
        .block => |b| return .{ .block = try block(allocator, builtins, locals, b) },
        .binary_op => |b| return try binaryOp(allocator, builtins, locals, b),
        .symbol => |s| return symbol(s),
        .call => |c| return try call(allocator, builtins, locals, c),
        .intrinsic => |i| return try intrinsic(allocator, builtins, locals, i),
        .if_else => |i| return try ifElse(allocator, builtins, locals, i),
        .cond => |c| return try cond(allocator, builtins, locals, c),
        .define => |d| return try define(allocator, builtins, locals, d),
        .convert => |c| return try convert(allocator, builtins, locals, c),
        else => |k| std.debug.panic("\nExpression {} not yet supported", .{k}),
    }
}

fn expressionAlloc(allocator: Allocator, builtins: Builtins, locals: *List(Local), e: type_checker.Expression) !*const Expression {
    const ptr = try allocator.create(Expression);
    ptr.* = try expression(allocator, builtins, locals, e);
    return ptr;
}

fn function(allocator: Allocator, builtins: Builtins, name: Interned, f: type_checker.Function) !Function {
    const parameters = try allocator.alloc(Parameter, f.parameters.len);
    for (f.parameters, parameters) |typed_p, *ir_p| {
        ir_p.* = Parameter{
            .name = typed_p.value,
            .type = mapType(typed_p.type),
        };
    }
    var locals = List(Local).init(allocator);
    const body = try block(allocator, builtins, &locals, f.body);
    return Function{
        .name = name,
        .parameters = parameters,
        .return_type = mapType(f.return_type),
        .locals = try locals.toOwnedSlice(),
        .body = body,
    };
}

fn foreignImport(allocator: Allocator, name: Interned, i: type_checker.ForeignImport) !Import {
    switch (i.type) {
        .function => |f| {
            const path = [2]Interned{ i.module, i.name };
            const function_type = try allocator.alloc(Type, f.len);
            for (f, function_type) |t, *ir_t| ir_t.* = mapType(t);
            return Import{
                .name = name,
                .path = path,
                .type = .{ .function = function_type },
            };
        },
        else => |k| std.debug.panic("\nForeign import type {} not yet supported", .{k}),
    }
}

pub fn buildIr(allocator: Allocator, builtins: Builtins, module: Ast) !IR {
    var functions = std.ArrayList(Function).init(allocator);
    var imports = std.ArrayList(Import).init(allocator);
    for (module.order) |name| {
        if (module.typed.get(name)) |top_level| {
            switch (top_level) {
                .define => |d| {
                    const name_symbol = d.name.value;
                    switch (d.value.*) {
                        .function => |f| {
                            const lowered = try function(allocator, builtins, name_symbol, f);
                            try functions.append(lowered);
                        },
                        .foreign_import => |i| {
                            const lowered = try foreignImport(allocator, name_symbol, i);
                            try imports.append(lowered);
                        },
                        else => |e| std.debug.panic("\nTop level kind {} no yet supported", .{e}),
                    }
                },
                else => std.debug.panic("\nTop level kind {} no yet supported", .{top_level}),
            }
        }
    }
    return IR{
        .functions = try functions.toOwnedSlice(),
        .imports = try imports.toOwnedSlice(),
        .exports = &.{},
    };
}
