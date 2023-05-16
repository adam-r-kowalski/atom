const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;

const Interned = @import("../interner.zig").Interned;
const parser_types = @import("../parser/types.zig");
const types = @import("types.zig");
const Module = types.Module;
const Untyped = types.Untyped;
const Typed = types.Typed;
const Scope = types.Scope;
const Scopes = types.Scopes;
const TopLevel = types.TopLevel;
const Function = types.Function;
const MonoType = types.MonoType;
const Symbol = types.Symbol;
const Int = types.Int;
const Float = types.Float;
const Bool = types.Bool;
const BinaryOp = types.BinaryOp;
const TypeVar = types.TypeVar;
const Expression = types.Expression;
const Constraints = types.Constraints;
const If = types.If;
const Define = types.Define;
const Call = types.Call;
const Builtins = @import("../builtins.zig").Builtins;

fn nameOf(top_level: parser_types.TopLevel) Interned {
    switch (top_level) {
        .function => |f| return f.name.value,
        .define => |d| return d.name.value,
        .import => |i| return i.declaration.name.value,
        .export_ => |e| return e.function.name.value,
    }
}

pub fn module(allocator: Allocator, m: parser_types.Module, next_type_var: *TypeVar) !Module {
    var order = List(Interned).init(allocator);
    var untyped = Untyped.init(allocator);
    var scope = Scope.init(allocator);
    for (m.top_level) |top_level| {
        const name = nameOf(top_level);
        try order.append(name);
        try untyped.putNoClobber(name, top_level);
        const monotype = freshTypeVar(next_type_var);
        try scope.put(name, monotype);
    }
    return Module{
        .order = order.toOwnedSlice(),
        .untyped = untyped,
        .typed = Typed.init(allocator),
        .scope = scope,
        .span = m.span,
        .type = .module,
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

const WorkQueue = Map(Interned, List(MonoType));

fn findInScope(allocator: Allocator, scopes: Scopes, work_queue: *WorkQueue, name: Interned) !MonoType {
    var i = scopes.items.len;
    while (i != 0) : (i -= 1) {
        if (scopes.items[i - 1].get(name)) |type_| {
            if (i != 1) return type_;
            const result = try work_queue.getOrPut(name);
            if (result.found_existing) return type_;
            result.value_ptr.* = List(MonoType).init(allocator);
            return type_;
        }
    }
    std.debug.panic("\nCould not find {} in scopes", .{name});
}

fn parameterType(builtins: Builtins, next_type_var: *TypeVar, p: parser_types.Parameter) !MonoType {
    return if (p.type) |t| expressionToMonoType(t, builtins) else freshTypeVar(next_type_var);
}

fn returnType(builtins: Builtins, next_type_var: *TypeVar, f: parser_types.Function) MonoType {
    return if (f.return_type) |t| expressionToMonoType(t.*, builtins) else freshTypeVar(next_type_var);
}

fn symbol(allocator: Allocator, scopes: Scopes, work_queue: *WorkQueue, s: parser_types.Symbol) !Symbol {
    return Symbol{
        .value = s.value,
        .span = s.span,
        .type = try findInScope(allocator, scopes, work_queue, s.value),
    };
}

fn int(i: parser_types.Int, next_type_var: *TypeVar) Int {
    return Int{ .value = i.value, .span = i.span, .type = freshTypeVar(next_type_var) };
}

fn float(f: parser_types.Float, next_type_var: *TypeVar) Float {
    return Float{ .value = f.value, .span = f.span, .type = freshTypeVar(next_type_var) };
}

fn boolean(b: parser_types.Bool) Bool {
    return Bool{ .value = b.value, .span = b.span, .type = .bool };
}

fn if_(allocator: Allocator, work_queue: *WorkQueue, builtins: Builtins, constraints: *Constraints, scopes: *Scopes, next_type_var: *TypeVar, i: parser_types.If) !If {
    const condition = try expressionAlloc(allocator, work_queue, builtins, constraints, scopes, next_type_var, i.condition.*);
    const then = try block(allocator, work_queue, builtins, constraints, scopes, next_type_var, i.then);
    const else_ = try block(allocator, work_queue, builtins, constraints, scopes, next_type_var, i.else_);
    const type_ = freshTypeVar(next_type_var);
    try constraints.equal.append(.{ .left = typeOf(condition.*), .right = .bool });
    try constraints.equal.append(.{ .left = typeOf(then[then.len - 1]), .right = type_ });
    try constraints.equal.append(.{ .left = typeOf(else_[else_.len - 1]), .right = type_ });
    return If{
        .condition = condition,
        .then = then,
        .else_ = else_,
        .type = type_,
        .span = i.span,
    };
}

fn binaryOp(allocator: Allocator, work_queue: *WorkQueue, builtins: Builtins, constraints: *Constraints, scopes: *Scopes, next_type_var: *TypeVar, b: parser_types.BinaryOp) !BinaryOp {
    const left = try expressionAlloc(allocator, work_queue, builtins, constraints, scopes, next_type_var, b.left.*);
    const right = try expressionAlloc(allocator, work_queue, builtins, constraints, scopes, next_type_var, b.right.*);
    const type_ = freshTypeVar(next_type_var);
    try constraints.equal.append(.{ .left = typeOf(left.*), .right = type_ });
    try constraints.equal.append(.{ .left = typeOf(right.*), .right = type_ });
    return BinaryOp{
        .kind = b.kind,
        .left = left,
        .right = right,
        .span = b.span,
        .type = type_,
    };
}

fn explicitTypeOrVar(builtins: Builtins, next_type_var: *TypeVar, e: ?*const parser_types.Expression) MonoType {
    return if (e) |t| expressionToMonoType(t.*, builtins) else freshTypeVar(next_type_var);
}

fn define(allocator: Allocator, work_queue: *WorkQueue, builtins: Builtins, constraints: *Constraints, scopes: *Scopes, next_type_var: *TypeVar, d: parser_types.Define) !Define {
    const body = try block(allocator, work_queue, builtins, constraints, scopes, next_type_var, d.body);
    const type_ = explicitTypeOrVar(builtins, next_type_var, d.type);
    try constraints.equal.append(.{ .left = typeOf(body[body.len - 1]), .right = type_ });
    const name = Symbol{
        .value = d.name.value,
        .span = d.name.span,
        .type = type_,
    };
    try putInScope(scopes, d.name.value, type_);
    return Define{
        .name = name,
        .body = body,
        .span = d.span,
        .type = .void,
    };
}

fn call(allocator: Allocator, work_queue: *WorkQueue, builtins: Builtins, constraints: *Constraints, scopes: *Scopes, next_type_var: *TypeVar, c: parser_types.Call) !Call {
    const f = try expressionAlloc(allocator, work_queue, builtins, constraints, scopes, next_type_var, c.function.*);
    const arguments = try allocator.alloc(Expression, c.arguments.len);
    const function_type = try allocator.alloc(MonoType, c.arguments.len + 1);
    for (c.arguments) |arg, i| {
        arguments[i] = try expression(allocator, work_queue, builtins, constraints, scopes, next_type_var, arg);
        function_type[i] = typeOf(arguments[i]);
    }
    const type_ = freshTypeVar(next_type_var);
    function_type[c.arguments.len] = type_;
    try constraints.equal.append(.{ .left = typeOf(f.*), .right = .{ .function = function_type } });
    return Call{
        .function = f,
        .arguments = arguments,
        .span = c.span,
        .type = type_,
    };
}

fn expression(allocator: Allocator, work_queue: *WorkQueue, builtins: Builtins, constraints: *Constraints, scopes: *Scopes, next_type_var: *TypeVar, expr: parser_types.Expression) error{OutOfMemory}!Expression {
    switch (expr) {
        .symbol => |s| return .{ .symbol = try symbol(allocator, scopes.*, work_queue, s) },
        .int => |i| return .{ .int = int(i, next_type_var) },
        .float => |f| return .{ .float = float(f, next_type_var) },
        .bool => |b| return .{ .bool = boolean(b) },
        .if_ => |i| return .{ .if_ = try if_(allocator, work_queue, builtins, constraints, scopes, next_type_var, i) },
        .binary_op => |b| return .{ .binary_op = try binaryOp(allocator, work_queue, builtins, constraints, scopes, next_type_var, b) },
        .define => |d| return .{ .define = try define(allocator, work_queue, builtins, constraints, scopes, next_type_var, d) },
        .call => |c| return .{ .call = try call(allocator, work_queue, builtins, constraints, scopes, next_type_var, c) },
        else => |e| std.debug.panic("\nUnsupported expression {}", .{e}),
    }
}

fn expressionAlloc(allocator: Allocator, work_queue: *WorkQueue, builtins: Builtins, constraints: *Constraints, scopes: *Scopes, next_type_var: *TypeVar, expr: parser_types.Expression) !*const Expression {
    const result = try allocator.create(Expression);
    result.* = try expression(allocator, work_queue, builtins, constraints, scopes, next_type_var, expr);
    return result;
}

fn block(allocator: Allocator, work_queue: *WorkQueue, builtins: Builtins, constraints: *Constraints, scopes: *Scopes, next_type_var: *TypeVar, exprs: []const parser_types.Expression) ![]const Expression {
    const expressions = try allocator.alloc(Expression, exprs.len);
    for (exprs) |expr, i|
        expressions[i] = try expression(allocator, work_queue, builtins, constraints, scopes, next_type_var, expr);
    return expressions;
}

fn typeOf(expr: Expression) MonoType {
    switch (expr) {
        .symbol => |s| return s.type,
        .int => |i| return i.type,
        .float => |f| return f.type,
        .bool => |b| return b.type,
        .if_ => |i| return i.type,
        .binary_op => |b| return b.type,
        .call => |c| return c.type,
        else => std.debug.panic("\nUnsupported expression {}", .{expr}),
    }
}

fn function(allocator: Allocator, work_queue: *WorkQueue, constraints: *Constraints, scopes: *Scopes, builtins: Builtins, next_type_var: *TypeVar, f: parser_types.Function) !Function {
    try pushScope(scopes);
    defer popScope(scopes);
    const parameters = try allocator.alloc(Symbol, f.parameters.len);
    const function_type = try allocator.alloc(MonoType, f.parameters.len + 1);
    for (f.parameters) |p, i| {
        const type_ = try parameterType(builtins, next_type_var, p);
        parameters[i] = Symbol{
            .value = p.name.value,
            .span = p.name.span,
            .type = type_,
        };
        function_type[i] = type_;
        try putInScope(scopes, p.name.value, type_);
    }
    const return_type = returnType(builtins, next_type_var, f);
    const body = try block(allocator, work_queue, builtins, constraints, scopes, next_type_var, f.body);
    try constraints.equal.append(.{
        .left = return_type,
        .right = typeOf(body[body.len - 1]),
    });
    function_type[f.parameters.len] = return_type;
    const name = Symbol{
        .value = f.name.value,
        .span = f.name.span,
        .type = .{ .function = function_type },
    };
    return Function{
        .name = name,
        .parameters = parameters,
        .return_type = return_type,
        .body = body,
        .span = f.span,
        .type = .void,
    };
}

fn topLevel(allocator: Allocator, work_queue: *WorkQueue, constraints: *Constraints, scope: Scope, builtins: Builtins, next_type_var: *TypeVar, t: parser_types.TopLevel) !TopLevel {
    switch (t) {
        .function => |f| {
            var scopes = Scopes.init(allocator);
            try scopes.append(scope);
            return .{ .function = try function(allocator, work_queue, constraints, &scopes, builtins, next_type_var, f) };
        },
        else => |e| std.debug.panic("\nUnsupported top level {}", .{e}),
    }
}

pub fn infer(allocator: Allocator, constraints: *Constraints, m: *Module, builtins: Builtins, next_type_var: *TypeVar, name: Interned) !void {
    var work_queue = WorkQueue.init(allocator);
    if (m.untyped.fetchRemove(name)) |entry| {
        const top_level = try topLevel(allocator, &work_queue, constraints, m.scope, builtins, next_type_var, entry.value);
        try m.typed.putNoClobber(name, top_level);
    }
}
