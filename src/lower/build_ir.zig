const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const types = @import("types.zig");
const IR = types.IR;
const Function = types.Function;
const Parameter = types.Parameter;
const Type = types.Type;
const Expression = types.Expression;
const Export = type_checker_types.Export;
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

fn block(allocator: Allocator, e: type_checker_types.Expression) !Expression {
    const b = e.kind.block;
    const expressions = try allocator.alloc(Expression, b.len);
    for (b, expressions) |expr, *ir_expr| {
        ir_expr.* = try expression(allocator, expr);
    }
    return Expression{ .block = expressions };
}

fn add(allocator: Allocator, b: type_checker_types.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, b.left.*);
    const right = try expressionAlloc(allocator, b.right.*);
    switch (b.left.type) {
        .i32 => return Expression{ .i32_add = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_add = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nAdd type {} not yet supported", .{k}),
    }
}

fn multiply(allocator: Allocator, b: type_checker_types.BinaryOp) !Expression {
    const left = try expressionAlloc(allocator, b.left.*);
    const right = try expressionAlloc(allocator, b.right.*);
    switch (b.left.type) {
        .i32 => return Expression{ .i32_mul = .{ .left = left, .right = right } },
        .f32 => return Expression{ .f32_mul = .{ .left = left, .right = right } },
        else => |k| std.debug.panic("\nMul type {} not yet supported", .{k}),
    }
}

fn binaryOp(allocator: Allocator, e: type_checker_types.Expression) !Expression {
    const b = e.kind.binary_op;
    switch (b.kind) {
        .add => return try add(allocator, b),
        .multiply => return try multiply(allocator, b),
        else => |k| std.debug.panic("\nBinary op {} not yet supported", .{k}),
    }
}

fn symbol(e: type_checker_types.Expression) Expression {
    return Expression{ .local_get = e.kind.symbol };
}

fn call(allocator: Allocator, e: type_checker_types.Expression) !Expression {
    const c = e.kind.call;
    const arguments = try allocator.alloc(Expression, c.arguments.len);
    for (c.arguments, arguments) |arg, *ir_arg| {
        ir_arg.* = try expression(allocator, arg);
    }
    return Expression{
        .call = .{
            .function = c.function.kind.symbol,
            .arguments = arguments,
        },
    };
}

fn expression(allocator: Allocator, e: type_checker_types.Expression) error{OutOfMemory}!Expression {
    switch (e.kind) {
        .int => return try int(e),
        .float => return try float(e),
        .block => return try block(allocator, e),
        .binary_op => return try binaryOp(allocator, e),
        .symbol => return symbol(e),
        .call => return try call(allocator, e),
        else => |k| std.debug.panic("\nExpression {} not yet supported", .{k}),
    }
}

fn expressionAlloc(allocator: Allocator, e: type_checker_types.Expression) !*const Expression {
    const ptr = try allocator.create(Expression);
    ptr.* = try expression(allocator, e);
    return ptr;
}

fn function(allocator: Allocator, name: Interned, f: type_checker_types.Function) !Function {
    const parameters = try allocator.alloc(Parameter, f.parameters.len);
    for (f.parameters, parameters) |typed_p, *ir_p| {
        ir_p.* = Parameter{
            .name = typed_p.kind.symbol,
            .type = mapType(typed_p.type),
        };
    }
    const body = try expressionAlloc(allocator, f.body.*);
    return Function{
        .name = name,
        .parameters = parameters,
        .return_type = mapType(f.return_type),
        .body = body,
    };
}

pub fn buildIr(allocator: Allocator, module: Module) !IR {
    var functions = std.ArrayList(Function).init(allocator);
    for (module.order) |name| {
        if (module.typed.get(name)) |top_level| {
            const d = top_level.kind.define;
            const name_symbol = d.name.kind.symbol;
            switch (d.value.kind) {
                .function => |f| {
                    const lowered = try function(allocator, name_symbol, f);
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
