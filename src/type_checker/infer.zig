const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

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
const TypeVar = types.TypeVar;
const Expression = types.Expression;
const Constraints = types.Constraints;
const If = types.If;
const Builtins = @import("../builtins.zig").Builtins;

fn nameOf(top_level: parser_types.TopLevel) Interned {
    switch (top_level) {
        .function => |f| return f.name.value,
        .define => |d| return d.name.value,
        .import => |i| return i.function.name.value,
        .export_ => |e| return e.function.name.value,
    }
}

pub fn module(allocator: Allocator, m: parser_types.Module) !Module {
    var order = List(Interned).init(allocator);
    var untyped = Untyped.init(allocator);
    for (m.top_level) |top_level| {
        const name = nameOf(top_level);
        try order.append(name);
        try untyped.putNoClobber(name, top_level);
    }
    return Module{
        .order = order.toOwnedSlice(),
        .untyped = untyped,
        .typed = Typed.init(allocator),
        .scope = Scope.init(allocator),
        .span = m.span,
        .type = .module,
    };
}

fn expressionToMonoType(e: parser_types.Expression, builtins: Builtins) MonoType {
    switch (e) {
        .symbol => |s| {
            if (s.value == builtins.i32) return .i32;
            std.debug.panic("\nCannot convert symbol {} to mono type", .{s});
        },
        else => std.debug.panic("\nCannot convert expression {} to mono type", .{e}),
    }
}

fn freshTypeVar(next_typevar: *TypeVar) MonoType {
    const typevar = next_typevar.*;
    next_typevar.* += 1;
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

fn findInScope(scopes: Scopes, name: Interned) MonoType {
    var i = scopes.items.len;
    while (i != 0) : (i -= 1)
        if (scopes.items[i - 1].get(name)) |type_| return type_;
    std.debug.panic("\nCould not find {} in scopes", .{name});
}

fn parameterType(builtins: Builtins, next_type_var: *TypeVar, p: parser_types.Parameter) !MonoType {
    return if (p.type) |t| expressionToMonoType(t, builtins) else freshTypeVar(next_type_var);
}

fn returnType(builtins: Builtins, next_type_var: *TypeVar, f: parser_types.Function) MonoType {
    return if (f.return_type) |t| expressionToMonoType(t.*, builtins) else freshTypeVar(next_type_var);
}

fn symbol(scopes: Scopes, s: parser_types.Symbol) Symbol {
    return Symbol{ .value = s.value, .span = s.span, .type = findInScope(scopes, s.value) };
}

fn if_(allocator: Allocator, constraints: *Constraints, scopes: *Scopes, next_type_var: *TypeVar, i: parser_types.If) !If {
    const condition = try expressionAlloc(allocator, constraints, scopes, next_type_var, i.condition.*);
    const then = try block(allocator, constraints, scopes, next_type_var, i.then);
    const else_ = try block(allocator, constraints, scopes, next_type_var, i.else_);
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

fn expression(allocator: Allocator, constraints: *Constraints, scopes: *Scopes, next_type_var: *TypeVar, expr: parser_types.Expression) error{OutOfMemory}!Expression {
    switch (expr) {
        .symbol => |s| return .{ .symbol = symbol(scopes.*, s) },
        .if_ => |i| return .{ .if_ = try if_(allocator, constraints, scopes, next_type_var, i) },
        else => |e| std.debug.panic("\nUnsupported expression {}", .{e}),
    }
}

fn expressionAlloc(allocator: Allocator, constraints: *Constraints, scopes: *Scopes, next_type_var: *TypeVar, expr: parser_types.Expression) !*const Expression {
    const result = try allocator.create(Expression);
    result.* = try expression(allocator, constraints, scopes, next_type_var, expr);
    return result;
}

fn block(allocator: Allocator, constraints: *Constraints, scopes: *Scopes, next_type_var: *TypeVar, exprs: []const parser_types.Expression) ![]const Expression {
    const expressions = try allocator.alloc(Expression, exprs.len);
    for (exprs) |expr, i|
        expressions[i] = try expression(allocator, constraints, scopes, next_type_var, expr);
    return expressions;
}

fn typeOf(expr: Expression) MonoType {
    switch (expr) {
        .symbol => |s| return s.type,
        .if_ => |i| return i.type,
        else => std.debug.panic("\nUnsupported expression {}", .{expr}),
    }
}

fn function(allocator: Allocator, constraints: *Constraints, scopes: *Scopes, builtins: Builtins, next_type_var: *TypeVar, f: parser_types.Function) !Function {
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
    const body = try block(allocator, constraints, scopes, next_type_var, f.body);
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

fn topLevel(allocator: Allocator, constraints: *Constraints, scope: Scope, builtins: Builtins, next_type_var: *TypeVar, t: parser_types.TopLevel) !TopLevel {
    switch (t) {
        .function => |f| {
            var scopes = Scopes.init(allocator);
            try scopes.append(scope);
            return .{ .function = try function(allocator, constraints, &scopes, builtins, next_type_var, f) };
        },
        else => |e| std.debug.panic("\nUnsupported top level {}", .{e}),
    }
}

pub fn infer(allocator: Allocator, constraints: *Constraints, m: *Module, builtins: Builtins, next_type_var: *TypeVar, name: Interned) !void {
    if (m.untyped.fetchRemove(name)) |entry| {
        const top_level = try topLevel(allocator, constraints, m.scope, builtins, next_type_var, entry.value);
        try m.typed.putNoClobber(name, top_level);
    }
}
