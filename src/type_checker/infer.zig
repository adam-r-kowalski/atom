const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoArrayHashMap;

const Interned = @import("../interner.zig").Interned;
const parser_types = @import("../parser/types.zig");
const Span = parser_types.Span;
const types = @import("types.zig");
const Int = types.Int;
const Float = types.Float;
const Bool = types.Bool;
const Symbol = types.Symbol;
const Module = types.Module;
const Untyped = types.Untyped;
const Typed = types.Typed;
const Scope = types.Scope;
const Scopes = types.Scopes;
const TopLevel = types.TopLevel;
const Function = types.Function;
const MonoType = types.MonoType;
const BinaryOp = types.BinaryOp;
const Block = types.Block;
const TypeVar = types.TypeVar;
const Expression = types.Expression;
const Constraints = types.Constraints;
const If = types.If;
const Define = types.Define;
const Call = types.Call;
const Equal = types.Equal;
const Builtins = @import("../builtins.zig").Builtins;
const parserSpanOf = @import("../parser/span.zig").span;

fn topLevelType(allocator: Allocator, builtins: Builtins, expr: parser_types.Expression) !MonoType {
    switch (expr) {
        .function => |f| {
            const len = f.parameters.len;
            const function_type = try allocator.alloc(MonoType, len + 1);
            for (f.parameters, function_type[0..len]) |p, *t|
                t.* = expressionToMonoType(p.type, builtins);
            function_type[len] = expressionToMonoType(f.return_type.*, builtins);
            return MonoType{ .function = function_type };
        },
        else => std.debug.panic("\nInvalid top level type {}", .{expr}),
    }
}

pub fn module(allocator: Allocator, builtins: Builtins, m: parser_types.Module) !Module {
    var order = List(Interned).init(allocator);
    var untyped = Untyped.init(allocator);
    var scope = Scope.init(allocator);
    for (m.expressions) |top_level| {
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
    return Module{
        .order = try order.toOwnedSlice(),
        .untyped = untyped,
        .typed = Typed.init(allocator),
        .scope = scope,
    };
}

fn expressionToMonoType(e: parser_types.Expression, builtins: Builtins) MonoType {
    switch (e) {
        .symbol => |s| {
            if (s.value == builtins.i32) return .i32;
            if (s.value == builtins.f32) return .f32;
            if (s.value == builtins.bool) return .bool;
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

fn symbol(scopes: Scopes, work_queue: *WorkQueue, s: parser_types.Symbol) !Symbol {
    return Symbol{
        .value = s.value,
        .span = s.span,
        .type = try findInScope(scopes, work_queue, s.value),
    };
}

fn int(i: parser_types.Int, next_type_var: *TypeVar) Int {
    return Int{
        .value = i.value,
        .span = i.span,
        .type = freshTypeVar(next_type_var),
    };
}

fn float(f: parser_types.Float, next_type_var: *TypeVar) Float {
    return Float{
        .value = f.value,
        .span = f.span,
        .type = freshTypeVar(next_type_var),
    };
}

fn boolean(b: parser_types.Bool) Bool {
    return Bool{
        .value = b.value,
        .span = b.span,
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

fn conditional(context: Context, i: parser_types.If) !If {
    const condition = try expressionAlloc(context, i.condition.*);
    const then = try block(context, i.then);
    const else_ = try block(context, i.else_);
    const type_ = freshTypeVar(context.next_type_var);
    try context.constraints.equal.appendSlice(&[_]Equal{
        .{ .left = typeOf(condition.*), .right = .bool },
        .{ .left = then.type, .right = type_ },
        .{ .left = else_.type, .right = type_ },
    });
    return If{
        .condition = condition,
        .then = then,
        .else_ = else_,
        .type = type_,
        .span = i.span,
    };
}

fn binaryOp(context: Context, b: parser_types.BinaryOp) !BinaryOp {
    const left = try expressionAlloc(context, b.left.*);
    const right = try expressionAlloc(context, b.right.*);
    const left_type = typeOf(left.*);
    try context.constraints.equal.append(.{ .left = left_type, .right = typeOf(right.*) });
    const result_type = blk: {
        switch (b.kind) {
            .equal, .greater => break :blk .bool,
            else => {
                const tvar = freshTypeVar(context.next_type_var);
                try context.constraints.equal.append(.{ .left = left_type, .right = tvar });
                break :blk tvar;
            },
        }
    };
    return BinaryOp{
        .kind = b.kind,
        .left = left,
        .right = right,
        .span = b.span,
        .type = result_type,
    };
}

fn explicitTypeOrVar(builtins: Builtins, next_type_var: *TypeVar, e: ?*const parser_types.Expression) MonoType {
    return if (e) |t| expressionToMonoType(t.*, builtins) else freshTypeVar(next_type_var);
}

fn typeOf(e: Expression) MonoType {
    return switch (e) {
        .int => |i| i.type,
        .float => |f| f.type,
        .symbol => |s| s.type,
        .bool => |b| b.type,
        .define => |d| d.type,
        .function => |f| f.type,
        .binary_op => |b| b.type,
        .group => |g| g.type,
        .block => |b| b.type,
        .if_ => |i| i.type,
        .call => |c| c.type,
    };
}

fn define(context: Context, d: parser_types.Define) !Define {
    const value = try expressionAlloc(context, d.value.*);
    const type_ = explicitTypeOrVar(context.builtins, context.next_type_var, d.type);
    try context.constraints.equal.append(.{ .left = typeOf(value.*), .right = type_ });
    const name = Symbol{
        .value = d.name.value,
        .span = d.span,
        .type = type_,
    };
    try putInScope(context.scopes, d.name.value, type_);
    return Define{
        .name = name,
        .value = value,
        .span = d.span,
        .type = .void,
    };
}

fn call(context: Context, c: parser_types.Call) !Call {
    const f = try expressionAlloc(context, c.function.*);
    const len = c.arguments.len;
    const arguments = try context.allocator.alloc(Expression, len);
    const function_type = try context.allocator.alloc(MonoType, len + 1);
    for (c.arguments, arguments, function_type[0..len]) |untyped_arg, *typed_arg, *t| {
        typed_arg.* = try expression(context, untyped_arg);
        t.* = typeOf(typed_arg.*);
    }
    const return_type = freshTypeVar(context.next_type_var);
    function_type[len] = return_type;
    try context.constraints.equal.append(.{
        .left = typeOf(f.*),
        .right = .{ .function = function_type },
    });
    return Call{
        .function = f,
        .arguments = arguments,
        .span = c.span,
        .type = return_type,
    };
}

fn function(context: Context, f: parser_types.Function) !Function {
    try pushScope(context.scopes);
    defer popScope(context.scopes);
    const len = f.parameters.len;
    const parameters = try context.allocator.alloc(Symbol, len);
    const function_type = try context.allocator.alloc(MonoType, len + 1);
    for (f.parameters, parameters, function_type[0..len]) |untyped_p, *typed_p, *t| {
        const name_symbol = untyped_p.name.value;
        const p_type = expressionToMonoType(untyped_p.type, context.builtins);
        const span = Span{
            .begin = untyped_p.name.span.begin,
            .end = parserSpanOf(untyped_p.type).end,
        };
        typed_p.* = Symbol{
            .value = name_symbol,
            .span = span,
            .type = p_type,
        };
        t.* = p_type;
        try putInScope(context.scopes, name_symbol, p_type);
    }
    const return_type = expressionToMonoType(f.return_type.*, context.builtins);
    const body = try block(context, f.body);
    try context.constraints.equal.append(.{ .left = return_type, .right = body.type });
    function_type[len] = return_type;
    return Function{
        .parameters = parameters,
        .return_type = return_type,
        .body = body,
        .span = f.span,
        .type = .{ .function = function_type },
    };
}

fn block(context: Context, b: parser_types.Block) !Block {
    const len = b.expressions.len;
    const expressions = try context.allocator.alloc(Expression, len);
    for (b.expressions, expressions) |untyped_e, *typed_e|
        typed_e.* = try expression(context, untyped_e);
    return Block{
        .expressions = expressions,
        .span = b.span,
        .type = typeOf(expressions[len - 1]),
    };
}

fn expression(context: Context, e: parser_types.Expression) error{OutOfMemory}!Expression {
    switch (e) {
        .int => |i| return .{ .int = int(i, context.next_type_var) },
        .float => |f| return .{ .float = float(f, context.next_type_var) },
        .symbol => |s| return .{ .symbol = try symbol(context.scopes.*, context.work_queue, s) },
        .bool => |b| return .{ .bool = boolean(b) },
        .define => |d| return .{ .define = try define(context, d) },
        .function => |f| return .{ .function = try function(context, f) },
        .binary_op => |b| return .{ .binary_op = try binaryOp(context, b) },
        .block => |b| return .{ .block = try block(context, b) },
        .if_ => |i| return .{ .if_ = try conditional(context, i) },
        .call => |c| return .{ .call = try call(context, c) },
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
