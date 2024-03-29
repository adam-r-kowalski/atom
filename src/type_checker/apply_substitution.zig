const std = @import("std");
const Map = std.AutoHashMap;
const Allocator = std.mem.Allocator;
const types = @import("types.zig");
const Parameter = @import("monotype.zig").Parameter;
const MonoType = @import("monotype.zig").MonoType;
const Fields = @import("monotype.zig").Fields;
const Interned = @import("../interner.zig").Interned;

pub fn monotype(allocator: Allocator, sub: types.Substitution, m: MonoType) !MonoType {
    switch (m) {
        .function => |f| {
            const parameters = try allocator.alloc(Parameter, f.parameters.len);
            for (f.parameters, parameters) |unapplied, *applied|
                applied.* = .{
                    .name = unapplied.name,
                    .type = try monotype(allocator, sub, unapplied.type),
                    .mutable = unapplied.mutable,
                };
            const return_type = try allocator.create(MonoType);
            return_type.* = try monotype(allocator, sub, f.return_type.*);
            return .{ .function = .{
                .parameters = parameters,
                .return_type = return_type,
                .span = f.span,
            } };
        },
        .array => |a| {
            const element_type = try allocator.create(MonoType);
            element_type.* = try monotype(allocator, sub, a.element_type.*);
            return .{ .array = .{
                .element_type = element_type,
                .span = a.span,
            } };
        },
        .typevar => |t| {
            if (sub.map.get(t.value)) |mono| {
                return mono;
            }
            return m;
        },
        else => return m,
    }
}

fn symbol(allocator: Allocator, sub: types.Substitution, s: types.Symbol) !types.Symbol {
    return .{
        .value = s.value,
        .span = s.span,
        .type = try monotype(allocator, sub, s.type),
        .binding = s.binding,
    };
}

fn int(allocator: Allocator, sub: types.Substitution, i: types.Int) !types.Int {
    return .{
        .value = i.value,
        .span = i.span,
        .type = try monotype(allocator, sub, i.type),
    };
}

fn float(allocator: Allocator, sub: types.Substitution, f: types.Float) !types.Float {
    return .{
        .value = f.value,
        .span = f.span,
        .type = try monotype(allocator, sub, f.type),
    };
}

fn branch(allocator: Allocator, sub: types.Substitution, b: types.Branch) !types.Branch {
    const arms = try allocator.alloc(types.Arm, b.arms.len);
    for (b.arms, arms) |unapplied, *applied| {
        applied.* = .{
            .condition = try expression(allocator, sub, unapplied.condition),
            .then = try block(allocator, sub, unapplied.then),
        };
    }
    return .{
        .arms = arms,
        .else_ = try block(allocator, sub, b.else_),
        .span = b.span,
        .type = try monotype(allocator, sub, b.type),
    };
}

fn binaryOp(allocator: Allocator, sub: types.Substitution, b: types.BinaryOp) !types.BinaryOp {
    return .{
        .kind = b.kind,
        .left = try expressionAlloc(allocator, sub, b.left.*),
        .right = try expressionAlloc(allocator, sub, b.right.*),
        .span = b.span,
        .type = try monotype(allocator, sub, b.type),
    };
}

fn dot(allocator: Allocator, sub: types.Substitution, d: types.Dot) !types.Dot {
    return .{
        .left = try expressionAlloc(allocator, sub, d.left.*),
        .right = try symbol(allocator, sub, d.right),
        .span = d.span,
        .type = try monotype(allocator, sub, d.type),
    };
}

fn define(allocator: Allocator, sub: types.Substitution, d: types.Define) !types.Define {
    return .{
        .name = try symbol(allocator, sub, d.name),
        .value = try expressionAlloc(allocator, sub, d.value.*),
        .span = d.span,
        .mutable = d.mutable,
        .type = try monotype(allocator, sub, d.type),
    };
}

fn drop(allocator: Allocator, sub: types.Substitution, d: types.Drop) !types.Drop {
    return .{
        .value = try expressionAlloc(allocator, sub, d.value.*),
        .span = d.span,
        .type = try monotype(allocator, sub, d.type),
    };
}

fn plusEqual(allocator: Allocator, sub: types.Substitution, p: types.PlusEqual) !types.PlusEqual {
    return .{
        .name = try symbol(allocator, sub, p.name),
        .value = try expressionAlloc(allocator, sub, p.value.*),
        .span = p.span,
        .type = try monotype(allocator, sub, p.type),
    };
}

fn timesEqual(allocator: Allocator, sub: types.Substitution, t: types.TimesEqual) !types.TimesEqual {
    return .{
        .name = try symbol(allocator, sub, t.name),
        .value = try expressionAlloc(allocator, sub, t.value.*),
        .span = t.span,
        .type = try monotype(allocator, sub, t.type),
    };
}

fn call(allocator: Allocator, sub: types.Substitution, c: types.Call) !types.Call {
    const arguments = try allocator.alloc(types.Argument, c.arguments.positional.len);
    for (c.arguments.positional, arguments) |unapplied, *applied|
        applied.* = .{
            .value = try expression(allocator, sub, unapplied.value),
            .mutable = unapplied.mutable,
        };
    var named_arguments = Map(Interned, types.Argument).init(allocator);
    for (c.arguments.named_order) |name| {
        const unapplied = c.arguments.named.get(name).?;
        const applied = .{
            .value = try expression(allocator, sub, unapplied.value),
            .mutable = unapplied.mutable,
        };
        try named_arguments.putNoClobber(name, applied);
    }
    return .{
        .function = try expressionAlloc(allocator, sub, c.function.*),
        .arguments = .{
            .positional = arguments,
            .named = named_arguments,
            .named_order = c.arguments.named_order,
            .span = c.arguments.span,
        },
        .span = c.span,
        .type = try monotype(allocator, sub, c.type),
    };
}

fn decorator(allocator: Allocator, sub: types.Substitution, d: types.Decorator) !types.Decorator {
    const arguments = try allocator.alloc(types.Argument, d.arguments.positional.len);
    for (d.arguments.positional, arguments) |unapplied, *applied|
        applied.* = .{
            .value = try expression(allocator, sub, unapplied.value),
            .mutable = unapplied.mutable,
        };
    var named_arguments = Map(Interned, types.Argument).init(allocator);
    for (d.arguments.named_order) |name| {
        const unapplied = d.arguments.named.get(name).?;
        const applied = .{
            .value = try expression(allocator, sub, unapplied.value),
            .mutable = unapplied.mutable,
        };
        try named_arguments.putNoClobber(name, applied);
    }
    return .{
        .attribute = d.attribute,
        .arguments = .{
            .positional = arguments,
            .named = named_arguments,
            .named_order = d.arguments.named_order,
            .span = d.arguments.span,
        },
        .span = d.span,
        .value = try expressionAlloc(allocator, sub, d.value.*),
        .type = try monotype(allocator, sub, d.type),
    };
}

fn intrinsic(allocator: Allocator, sub: types.Substitution, i: types.Intrinsic) !types.Intrinsic {
    const arguments = try allocator.alloc(types.Argument, i.arguments.len);
    for (i.arguments, arguments) |unapplied, *applied|
        applied.* = .{
            .value = try expression(allocator, sub, unapplied.value),
            .mutable = unapplied.mutable,
        };
    return .{
        .function = i.function,
        .arguments = arguments,
        .span = i.span,
        .type = try monotype(allocator, sub, i.type),
    };
}

fn block(allocator: Allocator, sub: types.Substitution, b: types.Block) !types.Block {
    const expressions = try allocator.alloc(types.Expression, b.expressions.len);
    for (b.expressions, expressions) |unapplied, *applied|
        applied.* = try expression(allocator, sub, unapplied);
    return .{
        .expressions = expressions,
        .span = b.span,
        .type = try monotype(allocator, sub, b.type),
    };
}

fn group(allocator: Allocator, sub: types.Substitution, g: types.Group) !types.Group {
    const expressions = try allocator.alloc(types.Expression, g.expressions.len);
    for (g.expressions, expressions) |unapplied, *applied|
        applied.* = try expression(allocator, sub, unapplied);
    return .{
        .expressions = expressions,
        .span = g.span,
        .type = try monotype(allocator, sub, g.type),
    };
}

fn function(allocator: Allocator, sub: types.Substitution, f: types.Function) !types.Function {
    const parameters = try allocator.alloc(types.Parameter, f.parameters.len);
    for (f.parameters, parameters) |unapplied, *applied|
        applied.* = .{
            .name = try symbol(allocator, sub, unapplied.name),
            .mutable = unapplied.mutable,
        };
    return .{
        .name = try symbol(allocator, sub, f.name),
        .parameters = parameters,
        .return_type = try monotype(allocator, sub, f.return_type),
        .body = try block(allocator, sub, f.body),
        .span = f.span,
        .type = try monotype(allocator, sub, f.type),
    };
}

fn prototype(allocator: Allocator, sub: types.Substitution, p: types.Prototype) !types.Prototype {
    const parameters = try allocator.alloc(types.Parameter, p.parameters.len);
    for (p.parameters, parameters) |unapplied, *applied|
        applied.* = .{
            .name = try symbol(allocator, sub, unapplied.name),
            .mutable = unapplied.mutable,
        };
    return .{
        .name = try symbol(allocator, sub, p.name),
        .parameters = parameters,
        .return_type = try monotype(allocator, sub, p.return_type),
        .span = p.span,
        .type = try monotype(allocator, sub, p.type),
    };
}

fn foreignExport(allocator: Allocator, sub: types.Substitution, f: types.ForeignExport) !types.ForeignExport {
    return .{
        .name = f.name,
        .function = try function(allocator, sub, f.function),
        .span = f.span,
        .type = try monotype(allocator, sub, f.type),
    };
}

fn undef(allocator: Allocator, sub: types.Substitution, u: types.Undefined) !types.Undefined {
    return .{
        .span = u.span,
        .type = try monotype(allocator, sub, u.type),
    };
}

fn structLiteral(allocator: Allocator, sub: types.Substitution, s: types.StructLiteral) !types.StructLiteral {
    var fields = types.Fields.init(allocator);
    for (s.order) |o| {
        const field = s.fields.get(o).?;
        const name = try symbol(allocator, sub, field.name);
        const value = try expression(allocator, sub, field.value);
        try fields.putNoClobber(o, .{
            .name = name,
            .value = value,
            .span = field.span,
        });
    }
    return .{
        .fields = fields,
        .order = s.order,
        .span = s.span,
        .type = try monotype(allocator, sub, s.type),
    };
}

fn array(allocator: Allocator, sub: types.Substitution, a: types.Array) !types.Array {
    const expressions = try allocator.alloc(types.Expression, a.expressions.len);
    for (a.expressions, expressions) |unapplied, *applied|
        applied.* = try expression(allocator, sub, unapplied);
    return .{
        .expressions = expressions,
        .span = a.span,
        .type = try monotype(allocator, sub, a.type),
    };
}

fn index(allocator: Allocator, sub: types.Substitution, i: types.Index) !types.Index {
    const expr = try expressionAlloc(allocator, sub, i.expression.*);
    const indices = try allocator.alloc(types.Expression, i.indices.len);
    for (i.indices, indices) |unapplied, *applied|
        applied.* = try expression(allocator, sub, unapplied);
    return .{
        .expression = expr,
        .indices = indices,
        .span = i.span,
        .type = try monotype(allocator, sub, i.type),
    };
}

fn templateLiteral(allocator: Allocator, sub: types.Substitution, t: types.TemplateLiteral) !types.TemplateLiteral {
    const arguments = try allocator.alloc(types.Expression, t.arguments.len);
    for (t.arguments, arguments) |unapplied, *applied|
        applied.* = try expression(allocator, sub, unapplied);
    const mono = try monotype(allocator, sub, t.type);
    if (t.function) |f| {
        return .{
            .function = try symbol(allocator, sub, f),
            .strings = t.strings,
            .arguments = arguments,
            .type = mono,
            .span = t.span,
        };
    }
    return .{
        .function = null,
        .strings = t.strings,
        .arguments = arguments,
        .type = mono,
        .span = t.span,
    };
}

pub fn expression(allocator: Allocator, sub: types.Substitution, e: types.Expression) error{OutOfMemory}!types.Expression {
    return switch (e) {
        .symbol => |s| .{ .symbol = try symbol(allocator, sub, s) },
        .int => |i| .{ .int = try int(allocator, sub, i) },
        .float => |f| .{ .float = try float(allocator, sub, f) },
        .bool => e,
        .string => e,
        .branch => |b| .{ .branch = try branch(allocator, sub, b) },
        .binary_op => |b| .{ .binary_op = try binaryOp(allocator, sub, b) },
        .dot => |d| .{ .dot = try dot(allocator, sub, d) },
        .define => |d| .{ .define = try define(allocator, sub, d) },
        .drop => |d| .{ .drop = try drop(allocator, sub, d) },
        .plus_equal => |p| .{ .plus_equal = try plusEqual(allocator, sub, p) },
        .times_equal => |t| .{ .times_equal = try timesEqual(allocator, sub, t) },
        .call => |c| .{ .call = try call(allocator, sub, c) },
        .decorator => |d| .{ .decorator = try decorator(allocator, sub, d) },
        .intrinsic => |i| .{ .intrinsic = try intrinsic(allocator, sub, i) },
        .function => |f| .{ .function = try function(allocator, sub, f) },
        .prototype => |p| .{ .prototype = try prototype(allocator, sub, p) },
        .block => |b| .{ .block = try block(allocator, sub, b) },
        .group => |g| .{ .group = try group(allocator, sub, g) },
        .foreign_import => e,
        .foreign_export => |f| .{ .foreign_export = try foreignExport(allocator, sub, f) },
        .convert => e,
        .undefined => |u| .{ .undefined = try undef(allocator, sub, u) },
        .array => |a| .{ .array = try array(allocator, sub, a) },
        .index => |i| .{ .index = try index(allocator, sub, i) },
        .template_literal => |t| .{ .template_literal = try templateLiteral(allocator, sub, t) },
    };
}

pub fn expressionAlloc(allocator: Allocator, sub: types.Substitution, e: types.Expression) !*const types.Expression {
    const expr = try allocator.create(types.Expression);
    expr.* = try expression(allocator, sub, e);
    return expr;
}

pub fn module(allocator: Allocator, sub: types.Substitution, m: types.Module) !types.Module {
    var typed = types.Typed.init(allocator);
    var iterator = m.typed.iterator();
    while (iterator.next()) |entry| {
        const applied = try expression(allocator, sub, entry.value_ptr.*);
        try typed.putNoClobber(entry.key_ptr.*, applied);
    }
    return types.Module{
        .allocator = allocator,
        .constraints = m.constraints,
        .builtins = m.builtins,
        .order = m.order,
        .untyped = m.untyped,
        .typed = typed,
        .scope = m.scope,
        .foreign_exports = m.foreign_exports,
    };
}
