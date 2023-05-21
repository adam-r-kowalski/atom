const std = @import("std");
const Allocator = std.mem.Allocator;

const types = @import("types.zig");
const Substitution = types.Substitution;
const Module = types.Module;
const MonoType = types.MonoType;
const Typed = types.Typed;
const Function = types.Function;
const Expression = types.Expression;
const If = types.If;
const BinaryOp = types.BinaryOp;
const Define = types.Define;
const Call = types.Call;

fn monotype(allocator: Allocator, s: Substitution, m: MonoType) !MonoType {
    switch (m) {
        .i32 => return .i32,
        .f32 => return .f32,
        .bool => return .bool,
        .void => return .void,
        .module => return .module,
        .function => |f| {
            const mapped = try allocator.alloc(MonoType, f.len);
            for (f) |t, i| mapped[i] = try monotype(allocator, s, t);
            return .{ .function = mapped };
        },
        .typevar => |t| {
            if (s.get(t)) |mono| return mono;
            return m;
        },
    }
}

fn symbol(allocator: Allocator, s: Substitution, e: Expression) !Expression {
    return Expression{
        .kind = .{ .symbol = e.kind.symbol },
        .span = e.span,
        .type = try monotype(allocator, s, e.type),
    };
}

fn int(allocator: Allocator, s: Substitution, e: Expression) !Expression {
    return Expression{
        .kind = .{ .int = e.kind.int },
        .span = e.span,
        .type = try monotype(allocator, s, e.type),
    };
}

fn float(allocator: Allocator, s: Substitution, e: Expression) !Expression {
    return Expression{
        .kind = .{ .float = e.kind.float },
        .span = e.span,
        .type = try monotype(allocator, s, e.type),
    };
}

fn if_(allocator: Allocator, s: Substitution, e: Expression) !Expression {
    const i = e.kind.if_;
    return Expression{
        .kind = .{
            .if_ = .{
                .condition = try expressionAlloc(allocator, s, i.condition.*),
                .then = try expressionAlloc(allocator, s, i.then.*),
                .else_ = try expressionAlloc(allocator, s, i.else_.*),
            },
        },
        .span = e.span,
        .type = try monotype(allocator, s, e.type),
    };
}

fn binaryOp(allocator: Allocator, s: Substitution, e: Expression) !Expression {
    const b = e.kind.binary_op;
    return Expression{
        .kind = .{
            .binary_op = .{
                .kind = b.kind,
                .left = try expressionAlloc(allocator, s, b.left.*),
                .right = try expressionAlloc(allocator, s, b.right.*),
            },
        },
        .span = e.span,
        .type = try monotype(allocator, s, e.type),
    };
}

fn define(allocator: Allocator, s: Substitution, e: Expression) !Expression {
    const d = e.kind.define;
    return Expression{
        .kind = .{
            .define = .{
                .name = try expressionAlloc(allocator, s, d.name.*),
                .value = try expressionAlloc(allocator, s, d.value.*),
            },
        },
        .span = e.span,
        .type = try monotype(allocator, s, e.type),
    };
}

fn call(allocator: Allocator, s: Substitution, e: Expression) !Expression {
    const c = e.kind.call;
    const arguments = try allocator.alloc(Expression, c.arguments.len);
    for (c.arguments) |a, i| arguments[i] = try expression(allocator, s, a);
    return Expression{
        .kind = .{
            .call = .{
                .function = try expressionAlloc(allocator, s, c.function.*),
                .arguments = arguments,
            },
        },
        .span = e.span,
        .type = try monotype(allocator, s, e.type),
    };
}

fn function(allocator: Allocator, s: Substitution, e: Expression) !Expression {
    const f = e.kind.function;
    const parameters = try allocator.alloc(Expression, f.parameters.len);
    for (f.parameters) |p, i| parameters[i] = try symbol(allocator, s, p);
    return Expression{
        .kind = .{
            .function = .{
                .parameters = parameters,
                .return_type = try monotype(allocator, s, f.return_type),
                .body = try expressionAlloc(allocator, s, f.body.*),
            },
        },
        .span = e.span,
        .type = try monotype(allocator, s, e.type),
    };
}

fn block(allocator: Allocator, s: Substitution, e: Expression) !Expression {
    const b = e.kind.block;
    const expressions = try allocator.alloc(Expression, b.len);
    for (b) |expr, i| expressions[i] = try expression(allocator, s, expr);
    return Expression{
        .kind = .{ .block = expressions },
        .span = e.span,
        .type = try monotype(allocator, s, e.type),
    };
}

fn expression(allocator: Allocator, s: Substitution, e: Expression) error{OutOfMemory}!Expression {
    switch (e.kind) {
        .symbol => return try symbol(allocator, s, e),
        .int => return try int(allocator, s, e),
        .float => return try float(allocator, s, e),
        .bool => return e,
        .if_ => return try if_(allocator, s, e),
        .binary_op => return try binaryOp(allocator, s, e),
        .define => return try define(allocator, s, e),
        .call => return try call(allocator, s, e),
        .function => return try function(allocator, s, e),
        .block => return try block(allocator, s, e),
        else => |k| std.debug.panic("\nUnsupported expression {}", .{k}),
    }
}

fn expressionAlloc(allocator: Allocator, s: Substitution, e: Expression) !*const Expression {
    const expr = try allocator.create(Expression);
    expr.* = try expression(allocator, s, e);
    return expr;
}

pub fn apply(allocator: Allocator, s: Substitution, m: Module) !Module {
    var typed = Typed.init(allocator);
    var iterator = m.typed.iterator();
    while (iterator.next()) |entry| {
        if (m.typed.get(entry.key_ptr.*)) |t| {
            const value = try expression(allocator, s, t);
            try typed.putNoClobber(entry.key_ptr.*, value);
        }
    }
    return Module{
        .order = m.order,
        .untyped = m.untyped,
        .typed = typed,
        .scope = m.scope,
    };
}
