const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoArrayHashMap;

const Interned = @import("../interner.zig").Interned;
const parser_types = @import("../parser/types.zig");
const Span = parser_types.Span;
const types = @import("types.zig");
const Module = types.Module;
const Untyped = types.Untyped;
const Typed = types.Typed;
const Scope = types.Scope;
const Scopes = types.Scopes;
const TopLevel = types.TopLevel;
const Function = types.Function;
const MonoType = types.MonoType;
const BinaryOp = types.BinaryOp;
const TypeVar = types.TypeVar;
const Expression = types.Expression;
const Constraints = types.Constraints;
const If = types.If;
const Define = types.Define;
const Call = types.Call;
const Equal = types.Equal;
const Builtins = @import("../builtins.zig").Builtins;

fn topLevelType(allocator: Allocator, builtins: Builtins, expr: parser_types.Expression) !MonoType {
    const f = expr.kind.function;
    const function_type = try allocator.alloc(MonoType, f.parameters.len + 1);
    for (f.parameters, 0..) |p, i|
        function_type[i] = expressionToMonoType(p.type.*, builtins);
    function_type[f.parameters.len] = expressionToMonoType(f.return_type.*, builtins);
    return MonoType{ .function = function_type };
}

pub fn module(allocator: Allocator, builtins: Builtins, m: parser_types.Module) !Module {
    var order = List(Interned).init(allocator);
    var untyped = Untyped.init(allocator);
    var scope = Scope.init(allocator);
    for (m.expressions) |top_level| {
        const d = top_level.kind.define;
        const name = d.name.kind.symbol;
        try order.append(name);
        try untyped.putNoClobber(name, top_level);
        const monotype = try topLevelType(allocator, builtins, d.value.*);
        try scope.put(name, monotype);
    }
    return Module{
        .order = try order.toOwnedSlice(),
        .untyped = untyped,
        .typed = Typed.init(allocator),
        .scope = scope,
    };
}

fn expressionToMonoType(e: parser_types.Expression, builtins: Builtins) MonoType {
    switch (e.kind) {
        .symbol => |s| {
            if (s == builtins.i32) return .i32;
            if (s == builtins.f32) return .f32;
            if (s == builtins.bool) return .bool;
            std.debug.panic("\nCannot convert symbol {} to mono type", .{s});
        },
        else => std.debug.panic("\nCannot convert expression {} to mono type", .{e}),
    }
}

fn freshTypeVar(next_type_var: *TypeVar) MonoType {
    const typevar = next_type_var.*;
    next_type_var.* += 1;
    return .{ .typevar = typevar };
}

fn pushScope(scopes: *Scopes) !void {
    try scopes.append(Scope.init(scopes.allocator));
}

fn popScope(scopes: *Scopes) void {
    _ = scopes.pop();
}

fn putInScope(scopes: *Scopes, name: Interned, type_: MonoType) !void {
    try scopes.items[scopes.items.len - 1].put(name, type_);
}

const WorkQueue = List(Interned);

fn findInScope(scopes: Scopes, work_queue: *WorkQueue, name: Interned) !MonoType {
    var i = scopes.items.len;
    while (i != 0) : (i -= 1) {
        if (scopes.items[i - 1].get(name)) |type_| {
            if (i == 1) try work_queue.append(name);
            return type_;
        }
    }
    std.debug.panic("\nCould not find {} in scopes", .{name});
}

fn returnType(builtins: Builtins, next_type_var: *TypeVar, f: parser_types.Function) MonoType {
    return if (f.return_type) |t| expressionToMonoType(t.*, builtins) else freshTypeVar(next_type_var);
}

fn symbol(scopes: Scopes, work_queue: *WorkQueue, e: parser_types.Expression) !Expression {
    const s = e.kind.symbol;
    return Expression{
        .kind = .{ .symbol = s },
        .span = e.span,
        .type = try findInScope(scopes, work_queue, s),
    };
}

fn int(e: parser_types.Expression, next_type_var: *TypeVar) Expression {
    return Expression{
        .kind = .{ .int = e.kind.int },
        .span = e.span,
        .type = freshTypeVar(next_type_var),
    };
}

fn float(e: parser_types.Expression, next_type_var: *TypeVar) Expression {
    return Expression{
        .kind = .{ .float = e.kind.float },
        .span = e.span,
        .type = freshTypeVar(next_type_var),
    };
}

fn boolean(e: parser_types.Expression) Expression {
    return Expression{
        .kind = .{ .bool = e.kind.bool },
        .span = e.span,
        .type = .bool,
    };
}

const Context = struct {
    allocator: Allocator,
    work_queue: *WorkQueue,
    builtins: Builtins,
    constraints: *Constraints,
    scopes: *Scopes,
    next_type_var: *TypeVar,
};

fn conditional(context: Context, e: parser_types.Expression) !Expression {
    const i = e.kind.if_;
    const condition = try expressionAlloc(context, i.condition.*);
    const then = try expressionAlloc(context, i.then.*);
    const else_ = try expressionAlloc(context, i.else_.*);
    const type_ = freshTypeVar(context.next_type_var);
    try context.constraints.equal.appendSlice(&[_]Equal{
        .{ .left = condition.type, .right = .bool },
        .{ .left = then.type, .right = type_ },
        .{ .left = else_.type, .right = type_ },
    });
    return Expression{
        .kind = .{ .if_ = .{ .condition = condition, .then = then, .else_ = else_ } },
        .type = type_,
        .span = e.span,
    };
}

fn binaryOp(context: Context, e: parser_types.Expression) !Expression {
    const b = e.kind.binary_op;
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    const type_ = freshTypeVar(context.next_type_var);
    try context.constraints.equal.appendSlice(&[_]Equal{
        .{ .left = left.type, .right = type_ },
        .{ .left = right.type, .right = type_ },
    });
    return Expression{
        .kind = .{ .binary_op = .{ .kind = b.kind, .left = left, .right = right } },
        .span = e.span,
        .type = type_,
    };
}

fn explicitTypeOrVar(builtins: Builtins, next_type_var: *TypeVar, e: ?*const parser_types.Expression) MonoType {
    return if (e) |t| expressionToMonoType(t.*, builtins) else freshTypeVar(next_type_var);
}

fn define(context: Context, e: parser_types.Expression) !Expression {
    const d = e.kind.define;
    const value = try expressionAlloc(context, d.value.*);
    const type_ = explicitTypeOrVar(context.builtins, context.next_type_var, d.type);
    try context.constraints.equal.append(.{ .left = value.type, .right = type_ });
    const name = try alloc(context, Expression{
        .kind = .{ .symbol = d.name.kind.symbol },
        .span = d.name.span,
        .type = type_,
    });
    try putInScope(context.scopes, d.name.kind.symbol, type_);
    return Expression{
        .kind = .{ .define = .{ .name = name, .value = value } },
        .span = e.span,
        .type = .void,
    };
}

fn call(context: Context, e: parser_types.Expression) !Expression {
    const c = e.kind.call;
    const f = try expressionAlloc(context, c.function.*);
    const arguments = try context.allocator.alloc(Expression, c.arguments.len);
    const function_type = try context.allocator.alloc(MonoType, c.arguments.len + 1);
    for (c.arguments, 0..) |arg, i| {
        arguments[i] = try expression(context, arg);
        function_type[i] = arguments[i].type;
    }
    const type_ = freshTypeVar(context.next_type_var);
    function_type[c.arguments.len] = type_;
    try context.constraints.equal.append(.{ .left = f.type, .right = .{ .function = function_type } });
    return Expression{
        .kind = .{ .call = .{ .function = f, .arguments = arguments } },
        .span = e.span,
        .type = type_,
    };
}

fn function(context: Context, e: parser_types.Expression) !Expression {
    const f = e.kind.function;
    try pushScope(context.scopes);
    defer popScope(context.scopes);
    const parameters = try context.allocator.alloc(Expression, f.parameters.len);
    const function_type = try context.allocator.alloc(MonoType, f.parameters.len + 1);
    for (f.parameters, 0..) |p, i| {
        const type_ = expressionToMonoType(p.type.*, context.builtins);
        const span = Span{
            .begin = p.name.span.begin,
            .end = p.type.span.end,
        };
        parameters[i] = Expression{
            .kind = .{ .symbol = p.name.kind.symbol },
            .span = span,
            .type = type_,
        };
        function_type[i] = type_;
        try putInScope(context.scopes, p.name.kind.symbol, type_);
    }
    const return_type = expressionToMonoType(f.return_type.*, context.builtins);
    const body = try expressionAlloc(context, f.body.*);
    try context.constraints.equal.append(.{ .left = return_type, .right = body.type });
    function_type[f.parameters.len] = return_type;
    return Expression{
        .kind = .{
            .function = .{
                .parameters = parameters,
                .return_type = return_type,
                .body = body,
            },
        },
        .span = e.span,
        .type = .{ .function = function_type },
    };
}

fn block(context: Context, e: parser_types.Expression) !Expression {
    const b = e.kind.block;
    const expressions = try context.allocator.alloc(Expression, b.len);
    for (b, 0..) |expr, i|
        expressions[i] = try expression(context, expr);
    return Expression{
        .kind = .{ .block = expressions },
        .span = e.span,
        .type = expressions[b.len - 1].type,
    };
}

fn expression(context: Context, e: parser_types.Expression) error{OutOfMemory}!Expression {
    switch (e.kind) {
        .int => return int(e, context.next_type_var),
        .float => return float(e, context.next_type_var),
        .symbol => return try symbol(context.scopes.*, context.work_queue, e),
        .bool => return boolean(e),
        .define => return try define(context, e),
        .function => return try function(context, e),
        .binary_op => return try binaryOp(context, e),
        .block => return try block(context, e),
        .if_ => return try conditional(context, e),
        .call => return try call(context, e),
        else => |k| std.debug.panic("\nUnsupported expression {}", .{k}),
    }
}

fn alloc(context: Context, expr: Expression) !*const Expression {
    const result = try context.allocator.create(Expression);
    result.* = expr;
    return result;
}

fn expressionAlloc(context: Context, expr: parser_types.Expression) !*const Expression {
    return try alloc(context, try expression(context, expr));
}

pub fn infer(allocator: Allocator, constraints: *Constraints, m: *Module, builtins: Builtins, next_type_var: *TypeVar, name: Interned) !void {
    var work_queue = WorkQueue.init(allocator);
    try work_queue.append(name);
    while (work_queue.items.len != 0) {
        const current = work_queue.pop();
        if (m.untyped.fetchRemove(current)) |entry| {
            var scopes = Scopes.init(allocator);
            try scopes.append(m.scope);
            const context = Context{
                .allocator = allocator,
                .work_queue = &work_queue,
                .builtins = builtins,
                .constraints = constraints,
                .scopes = &scopes,
                .next_type_var = next_type_var,
            };
            const expr = try expression(context, entry.value);
            try m.typed.putNoClobber(current, expr);
        }
    }
}
