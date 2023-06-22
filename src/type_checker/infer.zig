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
const MonoType = @import("monotype.zig").MonoType;
const withSpan = @import("monotype.zig").withSpan;

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
    try scopes.errors.undefined_variables.append(.{
        .symbol = s.value,
        .span = s.span,
        .in_scope = try in_scope.toOwnedSlice(),
    });
    return error.CompileError;
}

pub fn expressionToMonoType(allocator: Allocator, builtins: Builtins, e: parser.types.Expression) !types.MonoType {
    switch (e) {
        .symbol => |s| {
            if (s.value.eql(builtins.u8)) return .{ .u8 = .{ .span = s.span } };
            if (s.value.eql(builtins.i32)) return .{ .i32 = .{ .span = s.span } };
            if (s.value.eql(builtins.i64)) return .{ .i64 = .{ .span = s.span } };
            if (s.value.eql(builtins.f32)) return .{ .f32 = .{ .span = s.span } };
            if (s.value.eql(builtins.f64)) return .{ .f64 = .{ .span = s.span } };
            if (s.value.eql(builtins.bool)) return .{ .bool = .{ .span = s.span } };
            if (s.value.eql(builtins.void)) return .{ .void = .{ .span = s.span } };
            std.debug.panic("\nCannot convert symbol {} to mono type", .{s});
        },
        .prototype => |p| {
            const len = p.parameters.len;
            const parameters = try allocator.alloc(types.MonoType, len);
            for (p.parameters, parameters) |param, *t|
                t.* = try expressionToMonoType(allocator, builtins, param.type);
            const return_type = try allocator.create(MonoType);
            return_type.* = try expressionToMonoType(allocator, builtins, p.return_type.*);
            return types.MonoType{ .function = .{
                .parameters = parameters,
                .return_type = return_type,
                .span = p.span,
            } };
        },
        .array_of => |a| {
            if (a.size) |_| std.debug.panic("\nSize of array currently not supported", .{});
            const element_type = try allocator.create(types.MonoType);
            element_type.* = try expressionToMonoType(allocator, builtins, a.element_type.*);
            return types.MonoType{ .array = .{ .size = null, .element_type = element_type, .span = a.span } };
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
        .type = withSpan(binding.type, s.span),
        .global = binding.global,
    };
}

fn freshTypeVar(cs: *types.Constraints, span: ?types.Span) types.MonoType {
    const typevar = cs.next_type_var;
    cs.next_type_var += 1;
    return .{ .typevar = .{ .value = typevar, .span = span } };
}

fn int(context: Context, i: parser.types.Int) types.Int {
    return types.Int{
        .value = i.value,
        .span = i.span,
        .type = freshTypeVar(context.constraints, i.span),
    };
}

fn float(context: Context, f: parser.types.Float) types.Float {
    return types.Float{
        .value = f.value,
        .span = f.span,
        .type = freshTypeVar(context.constraints, f.span),
    };
}

fn string(context: Context, s: parser.types.String) !types.String {
    const element_type = try context.allocator.create(types.MonoType);
    element_type.* = .{ .u8 = .{ .span = null } };
    return types.String{
        .value = s.value,
        .span = s.span,
        .type = .{ .array = .{ .size = null, .element_type = element_type, .span = s.span } },
    };
}

fn boolean(b: parser.types.Bool) types.Bool {
    return types.Bool{
        .value = b.value,
        .span = b.span,
        .type = .{ .bool = .{ .span = b.span } },
    };
}

fn untypedUndefined(context: Context, u: parser.types.Undefined) types.Undefined {
    return types.Undefined{
        .span = u.span,
        .type = freshTypeVar(context.constraints, u.span),
    };
}

fn branch(context: Context, b: parser.types.Branch) !types.Branch {
    const arms = try context.allocator.alloc(types.Arm, b.arms.len);
    const result_type = freshTypeVar(context.constraints, b.span);
    for (arms, b.arms) |*typed_arm, untyped_arm| {
        const condition = try expression(context, untyped_arm.condition);
        const then = try block(context, untyped_arm.then);
        typed_arm.* = types.Arm{ .condition = condition, .then = then };
        try context.constraints.equal.appendSlice(&.{
            .{
                .left = typeOf(condition),
                .right = .{ .bool = .{ .span = null } },
            },
            .{ .left = then.type, .right = result_type },
        });
    }
    const else_ = try block(context, b.else_);
    try context.constraints.equal.append(.{ .left = else_.type, .right = result_type });
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
            const arguments = try context.allocator.alloc(parser.types.Argument, c.arguments.len + 1);
            arguments[0] = .{ .value = b.left.*, .mutable = false };
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
            try context.constraints.equal.append(.{ .left = typeOf(left.*), .right = typeOf(right.*) });
            return types.Expression{
                .binary_op = .{
                    .kind = b.kind,
                    .left = left,
                    .right = right,
                    .span = b.span,
                    .type = .{ .bool = .{ .span = b.span } },
                },
            };
        },
        else => {
            const left = try expressionAlloc(context, b.left.*);
            const right = try expressionAlloc(context, b.right.*);
            const left_type = typeOf(left.*);
            try context.constraints.equal.append(.{ .left = left_type, .right = typeOf(right.*) });
            const tvar = freshTypeVar(context.constraints, null);
            try context.constraints.equal.append(.{ .left = left_type, .right = tvar });
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
        try context.constraints.equal.append(.{ .left = annotated_type, .right = monotype });
        monotype = annotated_type;
    }
    const binding = types.Binding{
        .type = monotype,
        .global = false,
        .mutable = d.mutable,
    };
    const name = types.Symbol{
        .value = d.name.value,
        .span = d.span,
        .type = monotype,
        .global = false,
    };
    try putInScope(context.scopes, name.value, binding);
    return types.Define{
        .name = name,
        .value = value,
        .span = d.span,
        .mutable = d.mutable,
        .type = .{ .void = .{ .span = d.span } },
    };
}

fn drop(context: Context, d: parser.types.Drop) !types.Drop {
    const value = try expressionAlloc(context, d.value.*);
    var monotype = typeOf(value.*);
    if (d.type) |t| {
        const annotated_type = try expressionToMonoType(context.allocator, context.builtins, t.*);
        try context.constraints.equal.append(.{ .left = annotated_type, .right = monotype });
        monotype = annotated_type;
    }
    return types.Drop{
        .value = value,
        .span = d.span,
        .type = .{ .void = .{ .span = d.span } },
    };
}

fn plusEqual(context: Context, p: parser.types.PlusEqual) !types.PlusEqual {
    const value = try expressionAlloc(context, p.value.*);
    const binding = try findInScope(context.scopes.*, p.name);
    if (binding.global) std.debug.panic("Cannot reassign global variable {s}", .{p.name.value.string()});
    if (!binding.mutable) std.debug.panic("Cannot reassign immutable variable {s}", .{p.name.value.string()});
    try context.constraints.equal.append(.{ .left = typeOf(value.*), .right = binding.type });
    const name = types.Symbol{
        .value = p.name.value,
        .span = p.span,
        .type = binding.type,
        .global = binding.global,
    };
    return types.PlusEqual{
        .name = name,
        .value = value,
        .span = p.span,
        .type = .{ .void = .{ .span = p.span } },
    };
}

fn timesEqual(context: Context, t: parser.types.TimesEqual) !types.TimesEqual {
    const value = try expressionAlloc(context, t.value.*);
    const binding = try findInScope(context.scopes.*, t.name);
    if (binding.global) std.debug.panic("Cannot reassign global variable {s}", .{t.name.value.string()});
    if (!binding.mutable) std.debug.panic("Cannot reassign immutable variable {s}", .{t.name.value.string()});
    try context.constraints.equal.append(.{ .left = typeOf(value.*), .right = binding.type });
    const name = types.Symbol{
        .value = t.name.value,
        .span = t.span,
        .type = binding.type,
        .global = binding.global,
    };
    return types.TimesEqual{
        .name = name,
        .value = value,
        .span = t.span,
        .type = .{ .void = .{ .span = t.span } },
    };
}

fn callForeignImport(context: Context, c: parser.types.Call) !types.Expression {
    if (c.arguments.len != 3) std.debug.panic("foreign_import takes 3 arguments", .{});
    const monotype = try expressionToMonoType(context.allocator, context.builtins, c.arguments[2].value);
    return types.Expression{
        .foreign_import = .{
            .module = c.arguments[0].value.string.value,
            .name = c.arguments[1].value.string.value,
            .span = c.span,
            .type = monotype,
        },
    };
}

fn callForeignExport(context: Context, c: parser.types.Call) !types.Expression {
    if (c.arguments.len != 2) std.debug.panic("foreign_export takes 2 arguments", .{});
    return types.Expression{
        .foreign_export = .{
            .name = c.arguments[0].value.string.value,
            .value = try expressionAlloc(context, c.arguments[1].value),
            .span = c.span,
            .type = .{ .void = .{ .span = c.span } },
        },
    };
}

fn callConvert(context: Context, c: parser.types.Call) !types.Expression {
    if (c.arguments.len != 2) std.debug.panic("convert takes 2 arguments", .{});
    const monotype = try expressionToMonoType(context.allocator, context.builtins, c.arguments[1].value);
    return types.Expression{
        .convert = .{
            .value = try expressionAlloc(context, c.arguments[0].value),
            .span = c.span,
            .type = monotype,
        },
    };
}

fn callSqrt(context: Context, c: parser.types.Call) !types.Expression {
    if (c.arguments.len != 1) std.debug.panic("sqrt takes 1 arguments", .{});
    const arguments = try context.allocator.alloc(types.Expression, 1);
    arguments[0] = try expression(context, c.arguments[0].value);
    return types.Expression{
        .intrinsic = .{
            .function = context.builtins.sqrt,
            .arguments = arguments,
            .span = c.span,
            .type = typeOf(arguments[0]),
        },
    };
}

fn sizeOfMonoType(builtins: Builtins, m: types.MonoType) Interned {
    switch (m) {
        .u8 => return builtins.one,
        else => std.debug.panic("sizeOfMonoType: {} not yet implemented", .{m}),
    }
}

fn callEmpty(context: Context, c: parser.types.Call) !types.Expression {
    if (c.arguments.len != 2) std.debug.panic("empty takes 2 arguments", .{});
    const arguments = try context.allocator.alloc(types.Expression, 2);
    const arg = c.arguments[0].value;
    const monotype = try expressionToMonoType(context.allocator, context.builtins, arg);
    const arg_span = parser.span.expression(arg);
    arguments[0] = .{ .int = .{
        .value = sizeOfMonoType(context.builtins, monotype),
        .span = arg_span,
        .type = .{ .i32 = .{ .span = arg_span } },
    } };
    arguments[1] = try expression(context, c.arguments[1].value);
    try context.constraints.equal.append(.{
        .left = typeOf(arguments[1]),
        .right = .{ .i32 = .{ .span = null } },
    });
    const element_type = try context.allocator.create(types.MonoType);
    element_type.* = monotype;
    return types.Expression{ .intrinsic = .{
        .function = context.builtins.empty,
        .arguments = arguments,
        .span = c.span,
        .type = .{ .array = .{ .size = null, .element_type = element_type, .span = c.span } },
    } };
}

fn call(context: Context, c: parser.types.Call) !types.Expression {
    switch (c.function.*) {
        .symbol => |s| {
            if (s.value.eql(context.builtins.foreign_import)) return try callForeignImport(context, c);
            if (s.value.eql(context.builtins.foreign_export)) return try callForeignExport(context, c);
            if (s.value.eql(context.builtins.convert)) return try callConvert(context, c);
            if (s.value.eql(context.builtins.sqrt)) return try callSqrt(context, c);
            if (s.value.eql(context.builtins.empty)) return try callEmpty(context, c);
            const f = try symbol(context.scopes.*, s);
            const len = c.arguments.len;
            const parameters = try context.allocator.alloc(types.MonoType, len);
            const arguments = try context.allocator.alloc(types.Argument, len);
            for (c.arguments, arguments, parameters) |untyped_arg, *typed_arg, *parameter| {
                typed_arg.value = try expression(context, untyped_arg.value);
                typed_arg.mutable = untyped_arg.mutable;
                parameter.* = typeOf(typed_arg.value);
            }
            const return_type = try context.allocator.create(MonoType);
            return_type.* = freshTypeVar(context.constraints, null);
            try context.constraints.equal.append(.{
                .left = f.type,
                .right = .{ .function = .{ .parameters = parameters, .return_type = return_type, .span = null } },
            });
            return types.Expression{
                .call = .{
                    .function = try alloc(context.allocator, .{ .symbol = f }),
                    .arguments = arguments,
                    .span = c.span,
                    .type = return_type.*,
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
    const parameters = try context.allocator.alloc(types.Parameter, len);
    const function_parameters = try context.allocator.alloc(types.MonoType, len);
    for (f.parameters, parameters, function_parameters) |untyped_p, *typed_p, *t| {
        const name_symbol = untyped_p.name.value;
        const p_type = try expressionToMonoType(context.allocator, context.builtins, untyped_p.type);
        const span = parser.types.Span{
            .begin = untyped_p.name.span.begin,
            .end = parser.span.expression(untyped_p.type).end,
        };
        const binding = types.Binding{
            .type = p_type,
            .global = false,
            .mutable = untyped_p.mutable,
        };
        typed_p.* = types.Parameter{
            .name = types.Symbol{
                .value = name_symbol,
                .span = span,
                .type = p_type,
                .global = false,
            },
            .mutable = binding.mutable,
        };
        try putInScope(context.scopes, name_symbol, binding);
        t.* = p_type;
    }
    const return_type = try context.allocator.create(MonoType);
    return_type.* = try expressionToMonoType(context.allocator, context.builtins, f.return_type.*);
    const body = try block(context, f.body);
    try context.constraints.equal.append(.{
        .left = return_type.*,
        .right = body.type,
    });
    return types.Function{
        .parameters = parameters,
        .return_type = return_type.*,
        .body = body,
        .span = f.span,
        .type = .{ .function = .{ .parameters = function_parameters, .return_type = return_type, .span = f.span } },
    };
}

fn block(context: Context, b: parser.types.Block) !types.Block {
    const len = b.expressions.len;
    const expressions = try context.allocator.alloc(types.Expression, len);
    for (b.expressions, expressions) |untyped_e, *typed_e| {
        typed_e.* = try expression(context, untyped_e);
    }
    if (len == 0) {
        return types.Block{
            .expressions = expressions,
            .span = b.span,
            .type = .{ .void = .{ .span = b.span } },
        };
    }
    const last_type = typeOf(expressions[len - 1]);
    const block_type = freshTypeVar(context.constraints, b.span);
    try context.constraints.equal.append(.{ .left = block_type, .right = last_type });
    return types.Block{
        .expressions = expressions,
        .span = b.span,
        .type = block_type,
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
        .drop => |d| return .{ .drop = try drop(context, d) },
        .plus_equal => |a| return .{ .plus_equal = try plusEqual(context, a) },
        .times_equal => |a| return .{ .times_equal = try timesEqual(context, a) },
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
    const parameters = try allocator.alloc(types.MonoType, len);
    for (f.parameters, parameters) |p, *t|
        t.* = try expressionToMonoType(allocator, builtins, p.type);
    const return_type = try allocator.create(MonoType);
    return_type.* = try expressionToMonoType(allocator, builtins, f.return_type.*);
    return types.MonoType{ .function = .{
        .parameters = parameters,
        .return_type = return_type,
        .span = f.span,
    } };
}

fn topLevelCall(allocator: Allocator, builtins: Builtins, c: parser.types.Call) !types.MonoType {
    switch (c.function.*) {
        .symbol => |s| {
            if (s.value.eql(builtins.foreign_import)) {
                if (c.arguments.len != 3) std.debug.panic("foreign_import takes 3 arguments", .{});
                return try expressionToMonoType(allocator, builtins, c.arguments[2].value);
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
                            switch (c.arguments[0].value) {
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
