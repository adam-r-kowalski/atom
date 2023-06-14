const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const Interned = @import("../interner.zig").Interned;
const Builtins = @import("../builtins.zig").Builtins;
const types = @import("types.zig");
const spanOf = @import("span.zig").expression;
const typeOf = @import("type_of.zig").expression;
const parser = @import("../parser.zig");
const Errors = @import("../error_reporter.zig").types.Errors;

pub const WorkQueue = List(Interned);

pub const Scopes = struct {
    allocator: Allocator,
    base: types.Scope,
    active: *List(types.Scope),
    work_queue: *WorkQueue,
    errors: *Errors,
};

fn pushScope(scopes: *Scopes) !void {
    try scopes.active.append(types.Scope.init(scopes.allocator));
}

fn popScope(scopes: *Scopes) void {
    _ = scopes.active.pop();
}

fn putInScope(scopes: *Scopes, name: Interned, binding: types.Binding) !void {
    try scopes.active.items[scopes.active.items.len - 1].put(name, binding);
}

pub fn findInScope(scopes: Scopes, s: parser.types.Symbol) !types.Binding {
    var reverse_iterator = std.mem.reverseIterator(scopes.active.items);
    while (reverse_iterator.next()) |scope| {
        if (scope.get(s.value)) |binding| return binding;
    }
    if (scopes.base.get(s.value)) |binding| {
        try scopes.work_queue.append(s.value);
        return binding;
    }
    var in_scope = List(Interned).init(scopes.allocator);
    var base_iterator = scopes.base.keyIterator();
    while (base_iterator.next()) |key| try in_scope.append(key.*);
    for (scopes.active.items) |scope| {
        var scope_iterator = scope.keyIterator();
        while (scope_iterator.next()) |key| try in_scope.append(key.*);
    }
    try scopes.errors.errors.append(.{
        .undefined_variable = .{
            .symbol = s.value,
            .span = s.span,
            .in_scope = try in_scope.toOwnedSlice(),
        },
    });
    return error.CompileError;
}

pub fn expressionToMonoType(allocator: Allocator, builtins: Builtins, e: parser.types.Expression) !types.MonoType {
    switch (e) {
        .symbol => |s| {
            if (s.value.eql(builtins.u8)) return .u8;
            if (s.value.eql(builtins.i32)) return .i32;
            if (s.value.eql(builtins.i64)) return .i64;
            if (s.value.eql(builtins.f32)) return .f32;
            if (s.value.eql(builtins.f64)) return .f64;
            if (s.value.eql(builtins.bool)) return .bool;
            if (s.value.eql(builtins.void)) return .void;
            std.debug.panic("\nCannot convert symbol {} to mono type", .{s});
        },
        .prototype => |p| {
            const len = p.parameters.len;
            const function_type = try allocator.alloc(types.MonoType, len + 1);
            for (p.parameters, function_type[0..len]) |param, *t|
                t.* = try expressionToMonoType(allocator, builtins, param.type);
            function_type[len] = try expressionToMonoType(allocator, builtins, p.return_type.*);
            return types.MonoType{ .function = function_type };
        },
        .array_of => |a| {
            if (a.size) |_| std.debug.panic("\nSize of array currently not supported", .{});
            const element_type = try allocator.create(types.MonoType);
            element_type.* = try expressionToMonoType(allocator, builtins, a.element_type.*);
            return types.MonoType{ .array = .{ .size = null, .element_type = element_type } };
        },
        else => std.debug.panic("\nCannot convert expression {} to mono type", .{e}),
    }
}

const Context = struct {
    allocator: Allocator,
    builtins: Builtins,
    constraints: *types.Constraints,
    scopes: *Scopes,
};

fn symbol(scopes: Scopes, s: parser.types.Symbol) !types.Symbol {
    const binding = try findInScope(scopes, s);
    return types.Symbol{
        .value = s.value,
        .span = s.span,
        .type = binding.type,
        .mutable = binding.mutable,
        .global = binding.global,
    };
}

fn freshTypeVar(cs: *types.Constraints) types.MonoType {
    const typevar = cs.next_type_var;
    cs.next_type_var = types.TypeVar{ .value = cs.next_type_var.value + 1 };
    return .{ .typevar = typevar };
}

fn int(context: Context, i: parser.types.Int) types.Int {
    return types.Int{
        .value = i.value,
        .span = i.span,
        .type = freshTypeVar(context.constraints),
    };
}

fn float(context: Context, f: parser.types.Float) types.Float {
    return types.Float{
        .value = f.value,
        .span = f.span,
        .type = freshTypeVar(context.constraints),
    };
}

fn string(context: Context, s: parser.types.String) !types.String {
    const element_type = try context.allocator.create(types.MonoType);
    element_type.* = .u8;
    return types.String{
        .value = s.value,
        .span = s.span,
        .type = .{ .array = .{ .size = null, .element_type = element_type } },
    };
}

fn boolean(b: parser.types.Bool) types.Bool {
    return types.Bool{
        .value = b.value,
        .span = b.span,
        .type = .bool,
    };
}

fn untypedUndefined(context: Context, u: parser.types.Undefined) types.Undefined {
    return types.Undefined{
        .span = u.span,
        .type = freshTypeVar(context.constraints),
    };
}

fn branch(context: Context, b: parser.types.Branch) !types.Branch {
    const arms = try context.allocator.alloc(types.Arm, b.arms.len);
    const result_type = freshTypeVar(context.constraints);
    for (arms, b.arms) |*typed_arm, untyped_arm| {
        const condition = try expression(context, untyped_arm.condition);
        const then = try block(context, untyped_arm.then);
        typed_arm.* = types.Arm{ .condition = condition, .then = then };
        try context.constraints.equal.appendSlice(&[_]types.EqualConstraint{
            .{
                .left = .{ .type = typeOf(condition), .span = spanOf(condition) },
                .right = .{ .type = .bool, .span = null },
            },
            .{
                .left = .{ .type = then.type, .span = then.span },
                .right = .{ .type = result_type, .span = null },
            },
        });
    }
    const else_ = try block(context, b.else_);
    try context.constraints.equal.append(.{
        .left = .{ .type = else_.type, .span = else_.span },
        .right = .{ .type = result_type, .span = null },
    });
    return types.Branch{
        .arms = arms,
        .else_ = else_,
        .type = result_type,
        .span = b.span,
    };
}

fn dotCall(context: Context, b: parser.types.BinaryOp) !types.Expression {
    switch (b.right.*) {
        .call => |c| {
            const arguments = try context.allocator.alloc(parser.types.Expression, c.arguments.len + 1);
            arguments[0] = b.left.*;
            @memcpy(arguments[1..], c.arguments);
            const new_call = parser.types.Call{
                .function = c.function,
                .arguments = arguments,
                .span = b.span,
            };
            return try call(context, new_call);
        },
        else => |k| std.debug.panic("Expected call after dot, got {}", .{k}),
    }
}

fn binaryOp(context: Context, b: parser.types.BinaryOp) !types.Expression {
    switch (b.kind) {
        .dot => return dotCall(context, b),
        .equal, .greater, .less => {
            const left = try expressionAlloc(context, b.left.*);
            const right = try expressionAlloc(context, b.right.*);
            try context.constraints.equal.append(.{
                .left = .{ .type = typeOf(left.*), .span = parser.span.expression(b.left.*) },
                .right = .{ .type = typeOf(right.*), .span = parser.span.expression(b.right.*) },
            });
            return types.Expression{
                .binary_op = .{
                    .kind = b.kind,
                    .left = left,
                    .right = right,
                    .span = b.span,
                    .type = .bool,
                },
            };
        },
        else => {
            const left = try expressionAlloc(context, b.left.*);
            const right = try expressionAlloc(context, b.right.*);
            const left_typed_span = .{ .type = typeOf(left.*), .span = parser.span.expression(b.left.*) };
            try context.constraints.equal.append(.{
                .left = left_typed_span,
                .right = .{ .type = typeOf(right.*), .span = parser.span.expression(b.right.*) },
            });
            const tvar = freshTypeVar(context.constraints);
            try context.constraints.equal.append(.{
                .left = left_typed_span,
                .right = .{ .type = tvar, .span = null },
            });
            return types.Expression{
                .binary_op = .{
                    .kind = b.kind,
                    .left = left,
                    .right = right,
                    .span = b.span,
                    .type = tvar,
                },
            };
        },
    }
}

fn define(context: Context, d: parser.types.Define) !types.Define {
    const value = try expressionAlloc(context, d.value.*);
    var monotype = typeOf(value.*);
    if (d.type) |t| {
        const annotated_type = try expressionToMonoType(context.allocator, context.builtins, t.*);
        try context.constraints.equal.append(.{
            .left = .{ .type = annotated_type, .span = parser.span.expression(t.*) },
            .right = .{ .type = monotype, .span = parser.span.expression(d.value.*) },
        });
        monotype = annotated_type;
    }
    const binding = types.Binding{
        .type = monotype,
        .global = false,
        .mutable = false,
    };
    const name = types.Symbol{
        .value = d.name.value,
        .span = d.span,
        .type = monotype,
        .global = false,
        .mutable = false,
    };
    try putInScope(context.scopes, name.value, binding);
    return types.Define{
        .name = name,
        .value = value,
        .span = d.span,
        .mutable = d.mutable,
        .type = .void,
    };
}

fn addAssign(context: Context, d: parser.types.AddAssign) !types.AddAssign {
    const value = try expressionAlloc(context, d.value.*);
    var monotype = typeOf(value.*);
    const binding = types.Binding{
        .type = monotype,
        .global = false,
        .mutable = false,
    };
    const name = types.Symbol{
        .value = d.name.value,
        .span = d.span,
        .type = monotype,
        .global = false,
        .mutable = false,
    };
    try putInScope(context.scopes, name.value, binding);
    return types.AddAssign{
        .name = name,
        .value = value,
        .span = d.span,
        .type = .void,
    };
}

fn callForeignImport(context: Context, c: parser.types.Call) !types.Expression {
    if (c.arguments.len != 3) std.debug.panic("foreign_import takes 3 arguments", .{});
    const monotype = try expressionToMonoType(context.allocator, context.builtins, c.arguments[2]);
    return types.Expression{
        .foreign_import = .{
            .module = c.arguments[0].string.value,
            .name = c.arguments[1].string.value,
            .span = c.span,
            .type = monotype,
        },
    };
}

fn callForeignExport(context: Context, c: parser.types.Call) !types.Expression {
    if (c.arguments.len != 2) std.debug.panic("foreign_export takes 2 arguments", .{});
    return types.Expression{
        .foreign_export = .{
            .name = c.arguments[0].string.value,
            .value = try expressionAlloc(context, c.arguments[1]),
            .span = c.span,
            .type = .void,
        },
    };
}

fn callConvert(context: Context, c: parser.types.Call) !types.Expression {
    if (c.arguments.len != 2) std.debug.panic("convert takes 2 arguments", .{});
    const monotype = try expressionToMonoType(context.allocator, context.builtins, c.arguments[1]);
    return types.Expression{
        .convert = .{
            .value = try expressionAlloc(context, c.arguments[0]),
            .span = c.span,
            .type = monotype,
        },
    };
}

fn callSqrt(context: Context, c: parser.types.Call) !types.Expression {
    if (c.arguments.len != 1) std.debug.panic("sqrt takes 1 arguments", .{});
    const arguments = try context.allocator.alloc(types.Expression, 1);
    arguments[0] = try expression(context, c.arguments[0]);
    return types.Expression{
        .intrinsic = .{
            .function = context.builtins.sqrt,
            .arguments = arguments,
            .span = c.span,
            .type = typeOf(arguments[0]),
        },
    };
}

fn call(context: Context, c: parser.types.Call) !types.Expression {
    switch (c.function.*) {
        .symbol => |s| {
            const len = c.arguments.len;
            const function_type = try context.allocator.alloc(types.MonoType, len + 1);
            if (s.value.eql(context.builtins.foreign_import)) return try callForeignImport(context, c);
            if (s.value.eql(context.builtins.foreign_export)) return try callForeignExport(context, c);
            if (s.value.eql(context.builtins.convert)) return try callConvert(context, c);
            if (s.value.eql(context.builtins.sqrt)) return try callSqrt(context, c);
            const f = try symbol(context.scopes.*, s);
            const arguments = try context.allocator.alloc(types.Expression, len);
            for (c.arguments, arguments, function_type[0..len]) |untyped_arg, *typed_arg, *t| {
                typed_arg.* = try expression(context, untyped_arg);
                t.* = typeOf(typed_arg.*);
            }
            const return_type = freshTypeVar(context.constraints);
            function_type[len] = return_type;
            try context.constraints.equal.append(.{
                .left = .{ .type = f.type, .span = f.span },
                .right = .{ .type = .{ .function = function_type }, .span = null },
            });
            return types.Expression{
                .call = .{
                    .function = try alloc(context.allocator, .{ .symbol = f }),
                    .arguments = arguments,
                    .span = c.span,
                    .type = return_type,
                },
            };
        },
        else => |k| std.debug.panic("\nInvalid call function type {}", .{k}),
    }
}

fn function(context: Context, f: parser.types.Function) !types.Function {
    try pushScope(context.scopes);
    defer popScope(context.scopes);
    const len = f.parameters.len;
    const parameters = try context.allocator.alloc(types.Symbol, len);
    const function_type = try context.allocator.alloc(types.MonoType, len + 1);
    for (f.parameters, parameters, function_type[0..len]) |untyped_p, *typed_p, *t| {
        const name_symbol = untyped_p.name.value;
        const p_type = try expressionToMonoType(context.allocator, context.builtins, untyped_p.type);
        const span = parser.types.Span{
            .begin = untyped_p.name.span.begin,
            .end = parser.span.expression(untyped_p.type).end,
        };
        const binding = types.Binding{
            .type = p_type,
            .global = false,
            .mutable = false,
        };
        typed_p.* = types.Symbol{
            .value = name_symbol,
            .span = span,
            .type = p_type,
            .global = false,
            .mutable = false,
        };
        try putInScope(context.scopes, name_symbol, binding);
        t.* = p_type;
    }
    const return_type = try expressionToMonoType(context.allocator, context.builtins, f.return_type.*);
    const body = try block(context, f.body);
    try context.constraints.equal.append(.{
        .left = .{ .type = return_type, .span = parser.span.expression(f.return_type.*) },
        .right = .{ .type = body.type, .span = body.span },
    });
    function_type[len] = return_type;
    return types.Function{
        .parameters = parameters,
        .return_type = return_type,
        .body = body,
        .span = f.span,
        .type = .{ .function = function_type },
    };
}

fn block(context: Context, b: parser.types.Block) !types.Block {
    const len = b.expressions.len;
    const expressions = try context.allocator.alloc(types.Expression, len);
    for (b.expressions, expressions) |untyped_e, *typed_e| {
        typed_e.* = try expression(context, untyped_e);
    }
    const monotype = if (len == 0) .void else typeOf(expressions[len - 1]);
    return types.Block{
        .expressions = expressions,
        .span = b.span,
        .type = monotype,
    };
}

fn expression(context: Context, e: parser.types.Expression) error{ OutOfMemory, CompileError }!types.Expression {
    switch (e) {
        .int => |i| return .{ .int = int(context, i) },
        .float => |f| return .{ .float = float(context, f) },
        .string => |s| return .{ .string = try string(context, s) },
        .symbol => |s| return .{ .symbol = try symbol(context.scopes.*, s) },
        .bool => |b| return .{ .bool = boolean(b) },
        .define => |d| return .{ .define = try define(context, d) },
        .add_assign => |a| return .{ .add_assign = try addAssign(context, a) },
        .function => |f| return .{ .function = try function(context, f) },
        .binary_op => |b| return try binaryOp(context, b),
        .block => |b| return .{ .block = try block(context, b) },
        .branch => |b| return .{ .branch = try branch(context, b) },
        .call => |c| return try call(context, c),
        .undefined => |u| return .{ .undefined = untypedUndefined(context, u) },
        else => |k| std.debug.panic("\nUnsupported expression {}", .{k}),
    }
}

fn alloc(allocator: Allocator, expr: types.Expression) !*types.Expression {
    const result = try allocator.create(types.Expression);
    result.* = expr;
    return result;
}

fn expressionAlloc(context: Context, expr: parser.types.Expression) !*types.Expression {
    return try alloc(context.allocator, try expression(context, expr));
}

pub fn topLevel(m: *types.Module, name: Interned, errors: *Errors) !void {
    var work_queue = WorkQueue.init(m.allocator);
    try work_queue.append(name);
    while (work_queue.items.len != 0) {
        const current = work_queue.pop();
        if (m.untyped.fetchRemove(current)) |entry| {
            var active = List(types.Scope).init(m.allocator);
            try active.append(types.Scope.init(m.allocator));
            var scopes = Scopes{
                .allocator = m.allocator,
                .work_queue = &work_queue,
                .active = &active,
                .base = m.scope,
                .errors = errors,
            };
            const context = Context{
                .allocator = m.allocator,
                .builtins = m.builtins,
                .constraints = m.constraints,
                .scopes = &scopes,
            };
            const expr = try expression(context, entry.value);
            try m.typed.putNoClobber(current, expr);
        }
    }
}

fn topLevelFunction(allocator: Allocator, builtins: Builtins, f: parser.types.Function) !types.MonoType {
    const len = f.parameters.len;
    const function_type = try allocator.alloc(types.MonoType, len + 1);
    for (f.parameters, function_type[0..len]) |p, *t|
        t.* = try expressionToMonoType(allocator, builtins, p.type);
    function_type[len] = try expressionToMonoType(allocator, builtins, f.return_type.*);
    return types.MonoType{ .function = function_type };
}

fn topLevelCall(allocator: Allocator, builtins: Builtins, c: parser.types.Call) !types.MonoType {
    switch (c.function.*) {
        .symbol => |s| {
            if (s.value.eql(builtins.foreign_import)) {
                if (c.arguments.len != 3) std.debug.panic("foreign_import takes 3 arguments", .{});
                return try expressionToMonoType(allocator, builtins, c.arguments[2]);
            }
        },
        else => |k| std.debug.panic("\nInvalid top level call function {}", .{k}),
    }
    std.debug.panic("\nInvalid top level call {}", .{c.function});
}

fn topLevelInt(allocator: Allocator, builtins: Builtins, d: parser.types.Define) !types.MonoType {
    if (d.type) |t| {
        return try expressionToMonoType(allocator, builtins, t.*);
    }
    std.debug.panic("\nInvalid top level int {}", .{d});
}

fn topLevelType(allocator: Allocator, builtins: Builtins, d: parser.types.Define) !types.MonoType {
    return switch (d.value.*) {
        .function => |f| try topLevelFunction(allocator, builtins, f),
        .call => |c| try topLevelCall(allocator, builtins, c),
        .int => try topLevelInt(allocator, builtins, d),
        else => |k| std.debug.panic("\nInvalid top level value {}", .{k}),
    };
}

pub fn module(allocator: Allocator, cs: *types.Constraints, builtins: Builtins, ast: parser.types.Module) !types.Module {
    var order = List(Interned).init(allocator);
    var untyped = types.Untyped.init(allocator);
    var typed = types.Typed.init(allocator);
    var scope = types.Scope.init(allocator);
    var foreign_exports = List(Interned).init(allocator);
    for (ast.expressions) |top_level| {
        switch (top_level) {
            .define => |d| {
                const name = d.name.value;
                try order.append(name);
                try untyped.putNoClobber(name, top_level);
                const monotype = try topLevelType(allocator, builtins, d);
                try scope.put(name, types.Binding{
                    .type = monotype,
                    .global = true,
                    .mutable = false,
                });
            },
            .call => |c| {
                switch (c.function.*) {
                    .symbol => |sym| {
                        if (sym.value.eql(builtins.foreign_export)) {
                            if (c.arguments.len != 2) std.debug.panic("\nInvalid foreign export call {}", .{c});
                            switch (c.arguments[0]) {
                                .string => |str| {
                                    try order.append(str.value);
                                    try untyped.putNoClobber(str.value, top_level);
                                    try foreign_exports.append(str.value);
                                },
                                else => |k| std.debug.panic("\nInvalid foreign export call {}", .{k}),
                            }
                        } else {
                            std.debug.panic("\nInvalid top level call to {}", .{sym});
                        }
                    },
                    else => |k| std.debug.panic("\nInvalid top level call {}", .{k}),
                }
            },
            else => |k| std.debug.panic("\nInvalid top level expression {}", .{k}),
        }
    }
    return types.Module{
        .allocator = allocator,
        .constraints = cs,
        .builtins = builtins,
        .order = try order.toOwnedSlice(),
        .untyped = untyped,
        .typed = typed,
        .scope = scope,
        .foreign_exports = try foreign_exports.toOwnedSlice(),
    };
}
