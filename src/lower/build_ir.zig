const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const types = @import("types.zig");
const IR = types.IR;
const Function = types.Function;
const Parameter = types.Parameter;
const Type = types.Type;
const Expression = types.Expression;
const Local = types.Local;
const Export = type_checker_types.Export;
const Builtins = @import("../builtins.zig").Builtins;
const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const type_checker_types = @import("../type_checker/types.zig");
const Module = type_checker_types.Module;
const MonoType = type_checker_types.MonoType;

fn mapType(monotype: MonoType) Type {
    switch (monotype) {
        .i32 => return .i32,
        .f32 => return .f32,
        .bool => return .i32,
        else => std.debug.panic("\nMonotype {} not yet supported", .{monotype}),
    }
}

fn int(e: type_checker_types.Expression) !Expression {
    const i = e.kind.int;
    switch (e.type) {
        .i32 => return .{ .i32_const = i },
        .f32 => return .{ .f32_const = i },
        else => |k| std.debug.panic("\nInt type {} not yet supported", .{k}),
    }
}

fn float(e: type_checker_types.Expression) !Expression {
    const f = e.kind.float;
    switch (e.type) {
        .f32 => return .{ .f32_const = f },
        else => |k| std.debug.panic("\nFloat type {} not yet supported", .{k}),
    }
}

fn boolean(builtins: Builtins, e: type_checker_types.Expression) !Expression {
    const b = e.kind.bool;
    return Expression{ .i32_const = if (b) builtins.one else builtins.zero };
}

fn block(allocator: Allocator, builtins: Builtins, locals: *List(Local), e: type_checker_types.Expression) !Expression {
    const b = e.kind.block;
    const expressions = try allocator.alloc(Expression, b.len);
    for (b, expressions) |expr, *ir_expr| {
        ir_expr.* = try expression(allocator, builtins, locals, expr);
    }
    return Expression{ .block = expressions };
}

fn add(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker_types.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.type) {
        .i32 => return Expression{ .i32_add = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_add = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nAdd type {} not yet supported", .{k}),
    }
}

fn subtract(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker_types.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.type) {
        .i32 => return Expression{ .i32_sub = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_sub = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nSubtract type {} not yet supported", .{k}),
    }
}

fn multiply(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker_types.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.type) {
        .i32 => return Expression{ .i32_mul = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_mul = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nMul type {} not yet supported", .{k}),
    }
}

fn modulo(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker_types.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.type) {
        .i32 => return Expression{ .i32_rem_s = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nModulo type {} not yet supported", .{k}),
    }
}

fn equal(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker_types.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.type) {
        .i32 => return Expression{ .i32_eq = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_eq = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nEqual type {} not yet supported", .{k}),
    }
}

fn binaryOr(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker_types.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.type) {
        .bool => return Expression{ .i32_or = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nOr type {} not yet supported", .{k}),
    }
}

fn greater(allocator: Allocator, builtins: Builtins, locals: *List(Local), b: type_checker_types.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, builtins, locals, b.left.*);
    const right = try expressionAlloc(allocator, builtins, locals, b.right.*);
    switch (b.left.type) {
        .i32 => return Expression{ .i32_gt_s = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_gt = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nGreater type {} not yet supported", .{k}),
    }
}

fn binaryOp(allocator: Allocator, builtins: Builtins, locals: *List(Local), e: type_checker_types.Expression) !Expression {
    const b = e.kind.binary_op;
    switch (b.kind) {
        .add => return try add(allocator, builtins, locals, b),
        .subtract => return try subtract(allocator, builtins, locals, b),
        .multiply => return try multiply(allocator, builtins, locals, b),
        .modulo => return try modulo(allocator, builtins, locals, b),
        .equal => return try equal(allocator, builtins, locals, b),
        .or_ => return try binaryOr(allocator, builtins, locals, b),
        .greater => return try greater(allocator, builtins, locals, b),
        else => |k| std.debug.panic("\nBinary op {} not yet supported", .{k}),
    }
}

fn symbol(e: type_checker_types.Expression) Expression {
    return Expression{ .local_get = e.kind.symbol };
}

fn call(allocator: Allocator, builtins: Builtins, locals: *List(Local), e: type_checker_types.Expression) !Expression {
    const c = e.kind.call;
    const arguments = try allocator.alloc(Expression, c.arguments.len);
    for (c.arguments, arguments) |arg, *ir_arg| {
        ir_arg.* = try expression(allocator, builtins, locals, arg);
    }
    return Expression{
        .call = .{
            .function = c.function.kind.symbol,
            .arguments = arguments,
        },
    };
}

fn conditional(allocator: Allocator, builtins: Builtins, locals: *List(Local), e: type_checker_types.Expression) !Expression {
    const i = e.kind.if_;
    const condition = try expressionAlloc(allocator, builtins, locals, i.condition.*);
    const then = try expressionAlloc(allocator, builtins, locals, i.then.*);
    const else_ = try expressionAlloc(allocator, builtins, locals, i.else_.*);
    return Expression{
        .if_ = .{
            .result = mapType(e.type),
            .condition = condition,
            .then = then,
            .else_ = else_,
        },
    };
}

fn define(allocator: Allocator, builtins: Builtins, locals: *List(Local), e: type_checker_types.Expression) !Expression {
    const d = e.kind.define;
    const name = d.name.kind.symbol;
    const value = try expressionAlloc(allocator, builtins, locals, d.value.*);
    try locals.append(Local{ .name = name, .type = mapType(d.name.type) });
    return Expression{ .local_set = .{ .name = name, .value = value } };
}

fn expression(allocator: Allocator, builtins: Builtins, locals: *List(Local), e: type_checker_types.Expression) error{OutOfMemory}!Expression {
    switch (e.kind) {
        .int => return try int(e),
        .float => return try float(e),
        .bool => return try boolean(builtins, e),
        .block => return try block(allocator, builtins, locals, e),
        .binary_op => return try binaryOp(allocator, builtins, locals, e),
        .symbol => return symbol(e),
        .call => return try call(allocator, builtins, locals, e),
        .if_ => return try conditional(allocator, builtins, locals, e),
        .define => return try define(allocator, builtins, locals, e),
        else => |k| std.debug.panic("\nExpression {} not yet supported", .{k}),
    }
}

fn expressionAlloc(allocator: Allocator, builtins: Builtins, locals: *List(Local), e: type_checker_types.Expression) !*const Expression {
    const ptr = try allocator.create(Expression);
    ptr.* = try expression(allocator, builtins, locals, e);
    return ptr;
}

fn function(allocator: Allocator, builtins: Builtins, name: Interned, f: type_checker_types.Function) !Function {
    const parameters = try allocator.alloc(Parameter, f.parameters.len);
    for (f.parameters, parameters) |typed_p, *ir_p| {
        ir_p.* = Parameter{
            .name = typed_p.kind.symbol,
            .type = mapType(typed_p.type),
        };
    }
    var locals = List(Local).init(allocator);
    const body = try expressionAlloc(allocator, builtins, &locals, f.body.*);
    return Function{
        .name = name,
        .parameters = parameters,
        .return_type = mapType(f.return_type),
        .locals = try locals.toOwnedSlice(),
        .body = body,
    };
}

pub fn buildIr(allocator: Allocator, builtins: Builtins, module: Module) !IR {
    var functions = std.ArrayList(Function).init(allocator);
    for (module.order) |name| {
        if (module.typed.get(name)) |top_level| {
            const d = top_level.kind.define;
            const name_symbol = d.name.kind.symbol;
            switch (d.value.kind) {
                .function => |f| {
                    const lowered = try function(allocator, builtins, name_symbol, f);
                    try functions.append(lowered);
                },
                else => |e| std.debug.panic("\nTop level kind {} no yet supported", .{e}),
            }
        } else {
            std.debug.panic("\nCould not find {} in module\n", .{name});
        }
    }
    return IR{
        .functions = try functions.toOwnedSlice(),
        .exports = &.{},
    };
}
