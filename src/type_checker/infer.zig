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

fn pushScope(_: *Scopes) !void {}

fn popScope(_: *Scopes) void {}

fn putInScope(_: *Scopes, _: Interned, _: MonoType) !void {}

fn parameterType(builtins: Builtins, next_type_var: *TypeVar, p: parser_types.Parameter) !MonoType {
    return if (p.type) |t| expressionToMonoType(t, builtins) else freshTypeVar(next_type_var);
}

fn returnType(allocator: Allocator, builtins: Builtins, next_type_var: *TypeVar, f: parser_types.Function) !*const MonoType {
    var result = try allocator.create(MonoType);
    result.* = if (f.return_type) |t| expressionToMonoType(t.*, builtins) else freshTypeVar(next_type_var);
    return result;
}

fn symbol(s: parser_types.Symbol) Symbol {
    // TODO: look up type from scopes
    return Symbol{
        .value = s.value,
        .span = s.span,
        .type = .void,
    };
}

fn expression(expr: parser_types.Expression) !Expression {
    switch (expr) {
        .symbol => |s| return .{ .symbol = symbol(s) },
        else => |e| std.debug.panic("\nUnsupported expression {}", .{e}),
    }
}

fn block(allocator: Allocator, exprs: []const parser_types.Expression) ![]const Expression {
    const expressions = try allocator.alloc(Expression, exprs.len);
    for (exprs) |expr, i|
        expressions[i] = try expression(expr);
    return expressions;
}

fn function(allocator: Allocator, scopes: *Scopes, builtins: Builtins, next_type_var: *TypeVar, f: parser_types.Function) !Function {
    try pushScope(scopes);
    defer popScope(scopes);
    const parameters = try allocator.alloc(Symbol, f.parameters.len);
    const parameterTypes = try allocator.alloc(MonoType, f.parameters.len);
    for (f.parameters) |p, i| {
        const type_ = try parameterType(builtins, next_type_var, p);
        parameters[i] = Symbol{
            .value = p.name.value,
            .span = p.name.span,
            .type = type_,
        };
        parameterTypes[i] = type_;
        try putInScope(scopes, p.name.value, type_);
    }
    const return_type = try returnType(allocator, builtins, next_type_var, f);
    const body = try block(allocator, f.body);
    const name = Symbol{
        .value = f.name.value,
        .span = f.name.span,
        .type = MonoType{ .function = .{
            .parameters = parameterTypes,
            .return_type = return_type,
        } },
    };
    return Function{
        .name = name,
        .parameters = parameters,
        .return_type = return_type.*,
        .body = body,
        .span = f.span,
        .type = .void,
    };
}

fn topLevel(allocator: Allocator, scope: Scope, builtins: Builtins, next_type_var: *TypeVar, t: parser_types.TopLevel) !TopLevel {
    switch (t) {
        .function => |f| {
            var scopes = Scopes.init(allocator);
            try scopes.append(scope);
            return .{ .function = try function(allocator, &scopes, builtins, next_type_var, f) };
        },
        else => |e| std.debug.panic("\nUnsupported top level {}", .{e}),
    }
}

pub fn infer(allocator: Allocator, m: *Module, builtins: Builtins, next_type_var: *TypeVar, name: Interned) !void {
    if (m.untyped.fetchRemove(name)) |entry| {
        const top_level = try topLevel(allocator, m.scope, builtins, next_type_var, entry.value);
        try m.typed.putNoClobber(name, top_level);
    }
}
