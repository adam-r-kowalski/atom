const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const Interned = @import("interner.zig").Interned;
const Builtins = @import("builtins.zig").Builtins;
const constraints = @import("constraints.zig");
const Constraints = constraints.Constraints;
const Equal = constraints.Equal;
const substitution = @import("substitution.zig");
const MonoType = substitution.MonoType;
const TypeVar = substitution.TypeVar;
const typed_ast = @import("typed_ast.zig");
const Scope = typed_ast.Scope;
const Scopes = typed_ast.Scopes;
const WorkQueue = typed_ast.WorkQueue;
const expressionToMonoType = typed_ast.expressionToMonoType;
const Symbol = typed_ast.Symbol;
const Int = typed_ast.Int;
const Float = typed_ast.Float;
const String = typed_ast.String;
const Bool = typed_ast.Bool;
const If = typed_ast.If;
const Cond = typed_ast.Cond;
const Expression = typed_ast.Expression;
const Block = typed_ast.Block;
const Define = typed_ast.Define;
const Function = typed_ast.Function;
const Module = typed_ast.Module;
const Span = typed_ast.Span;
const ast = @import("ast.zig");

const Context = struct {
    allocator: Allocator,
    builtins: Builtins,
    constraints: *Constraints,
    scopes: *Scopes,
};

fn freshTypeVar(next_type_var: *TypeVar) MonoType {
    const typevar = next_type_var.*;
    next_type_var.* += 1;
    return .{ .typevar = typevar };
}

fn returnType(builtins: Builtins, next_type_var: *TypeVar, f: ast.Function) MonoType {
    return if (f.return_type) |t| expressionToMonoType(t.*, builtins) else freshTypeVar(next_type_var);
}

fn symbol(scopes: Scopes, s: ast.Symbol) !Symbol {
    return Symbol{
        .value = s.value,
        .span = s.span,
        .type = try scopes.find(s.value),
    };
}

fn int(context: Context, i: ast.Int) Int {
    return Int{
        .value = i.value,
        .span = i.span,
        .type = context.constraints.freshTypeVar(),
    };
}

fn float(context: Context, f: ast.Float) Float {
    return Float{
        .value = f.value,
        .span = f.span,
        .type = context.constraints.freshTypeVar(),
    };
}

fn string(s: ast.String) String {
    return String{
        .value = s.value,
        .span = s.span,
        .type = .str,
    };
}

fn boolean(b: ast.Bool) Bool {
    return Bool{
        .value = b.value,
        .span = b.span,
        .type = .bool,
    };
}

fn ifElse(context: Context, i: ast.If) !If {
    const condition = try expressionAlloc(context, i.condition.*);
    const then = try block(context, i.then);
    const else_ = try block(context, i.else_);
    const type_ = context.constraints.freshTypeVar();
    try context.constraints.equal.appendSlice(&[_]Equal{
        .{ .left = condition.typeOf(), .right = .bool },
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

fn cond(context: Context, c: ast.Cond) !Cond {
    const conditions = try context.allocator.alloc(Expression, c.conditions.len);
    const thens = try context.allocator.alloc(Block, c.thens.len);
    const type_ = context.constraints.freshTypeVar();
    for (conditions, thens, c.conditions, c.thens) |*typed_c, *typed_t, untyped_c, untyped_t| {
        typed_c.* = try expression(context, untyped_c);
        typed_t.* = try block(context, untyped_t);
        try context.constraints.equal.appendSlice(&[_]Equal{
            .{ .left = typed_c.typeOf(), .right = .bool },
            .{ .left = typed_t.type, .right = type_ },
        });
    }
    const else_ = try block(context, c.else_);
    try context.constraints.equal.append(.{ .left = else_.type, .right = type_ });
    return Cond{
        .conditions = conditions,
        .thens = thens,
        .else_ = else_,
        .type = type_,
        .span = c.span,
    };
}

fn dotCall(context: Context, b: ast.BinaryOp) !Expression {
    switch (b.right.*) {
        .call => |c| {
            const arguments = try context.allocator.alloc(ast.Expression, c.arguments.len + 1);
            arguments[0] = b.left.*;
            @memcpy(arguments[1..], c.arguments);
            const new_call = ast.Call{
                .function = c.function,
                .arguments = arguments,
                .span = b.span,
            };
            return try call(context, new_call);
        },
        else => |k| std.debug.panic("Expected call after dot, got {}", .{k}),
    }
}

fn binaryOp(context: Context, b: ast.BinaryOp) !Expression {
    switch (b.kind) {
        .dot => return dotCall(context, b),
        .equal, .greater, .less => {
            const left = try expressionAlloc(context, b.left.*);
            const right = try expressionAlloc(context, b.right.*);
            const left_type = left.typeOf();
            try context.constraints.equal.append(.{ .left = left_type, .right = right.typeOf() });
            return Expression{
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
            const left_type = left.typeOf();
            try context.constraints.equal.append(.{ .left = left_type, .right = right.typeOf() });
            const tvar = context.constraints.freshTypeVar();
            try context.constraints.equal.append(.{ .left = left_type, .right = tvar });
            return Expression{
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

fn explicitTypeOrVar(context: Context, e: ?*const ast.Expression) !MonoType {
    if (e) |t|
        return try expressionToMonoType(context.allocator, context.builtins, t.*);
    return context.constraints.freshTypeVar();
}

fn define(context: Context, d: ast.Define) !Define {
    const value = try expressionAlloc(context, d.value.*);
    const monotype = try explicitTypeOrVar(context, d.type);
    try context.constraints.equal.append(.{ .left = value.typeOf(), .right = monotype });
    const name = Symbol{
        .value = d.name.value,
        .span = d.span,
        .type = monotype,
    };
    try context.scopes.put(d.name.value, monotype);
    return Define{
        .name = name,
        .value = value,
        .span = d.span,
        .type = .void,
    };
}

fn callForeignImport(context: Context, c: ast.Call) !Expression {
    if (c.arguments.len != 3) std.debug.panic("foreign_import takes 3 arguments", .{});
    const monotype = try expressionToMonoType(context.allocator, context.builtins, c.arguments[2]);
    return Expression{
        .foreign_import = .{
            .module = c.arguments[0].string.value,
            .name = c.arguments[1].string.value,
            .span = c.span,
            .type = monotype,
        },
    };
}

fn callConvert(context: Context, c: ast.Call) !Expression {
    if (c.arguments.len != 2) std.debug.panic("convert takes 2 arguments", .{});
    const monotype = try expressionToMonoType(context.allocator, context.builtins, c.arguments[1]);
    return Expression{
        .convert = .{
            .value = try expressionAlloc(context, c.arguments[0]),
            .span = c.span,
            .type = monotype,
        },
    };
}

fn callSqrt(context: Context, c: ast.Call) !Expression {
    if (c.arguments.len != 1) std.debug.panic("sqrt takes 1 arguments", .{});
    const arguments = try context.allocator.alloc(Expression, 1);
    arguments[0] = try expression(context, c.arguments[0]);
    return Expression{
        .intrinsic = .{
            .function = context.builtins.sqrt,
            .arguments = arguments,
            .span = c.span,
            .type = arguments[0].typeOf(),
        },
    };
}

fn call(context: Context, c: ast.Call) !Expression {
    switch (c.function.*) {
        .symbol => |s| {
            const len = c.arguments.len;
            const function_type = try context.allocator.alloc(MonoType, len + 1);
            if (s.value.eql(context.builtins.foreign_import)) return try callForeignImport(context, c);
            if (s.value.eql(context.builtins.convert)) return try callConvert(context, c);
            if (s.value.eql(context.builtins.sqrt)) return try callSqrt(context, c);
            const f = try symbol(context.scopes.*, s);
            const arguments = try context.allocator.alloc(Expression, len);
            for (c.arguments, arguments, function_type[0..len]) |untyped_arg, *typed_arg, *t| {
                typed_arg.* = try expression(context, untyped_arg);
                t.* = typed_arg.typeOf();
            }
            const return_type = context.constraints.freshTypeVar();
            function_type[len] = return_type;
            try context.constraints.equal.append(.{
                .left = f.type,
                .right = .{ .function = function_type },
            });
            return Expression{
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

fn function(context: Context, f: ast.Function) !Function {
    try context.scopes.push();
    defer context.scopes.pop();
    const len = f.parameters.len;
    const parameters = try context.allocator.alloc(Symbol, len);
    const function_type = try context.allocator.alloc(MonoType, len + 1);
    for (f.parameters, parameters, function_type[0..len]) |untyped_p, *typed_p, *t| {
        const name_symbol = untyped_p.name.value;
        const p_type = try expressionToMonoType(context.allocator, context.builtins, untyped_p.type);
        const span = Span{
            .begin = untyped_p.name.span.begin,
            .end = untyped_p.type.span().end,
        };
        typed_p.* = Symbol{
            .value = name_symbol,
            .span = span,
            .type = p_type,
        };
        t.* = p_type;
        try context.scopes.put(name_symbol, p_type);
    }
    const return_type = try expressionToMonoType(context.allocator, context.builtins, f.return_type.*);
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

fn block(context: Context, b: ast.Block) !Block {
    const len = b.expressions.len;
    const expressions = try context.allocator.alloc(Expression, len);
    for (b.expressions, expressions) |untyped_e, *typed_e|
        typed_e.* = try expression(context, untyped_e);
    const monotype = if (len == 0) .void else expressions[len - 1].typeOf();
    return Block{
        .expressions = expressions,
        .span = b.span,
        .type = monotype,
    };
}

fn expression(context: Context, e: ast.Expression) error{OutOfMemory}!Expression {
    switch (e) {
        .int => |i| return .{ .int = int(context, i) },
        .float => |f| return .{ .float = float(context, f) },
        .string => |s| return .{ .string = string(s) },
        .symbol => |s| return .{ .symbol = try symbol(context.scopes.*, s) },
        .bool => |b| return .{ .bool = boolean(b) },
        .define => |d| return .{ .define = try define(context, d) },
        .function => |f| return .{ .function = try function(context, f) },
        .binary_op => |b| return try binaryOp(context, b),
        .block => |b| return .{ .block = try block(context, b) },
        .if_else => |i| return .{ .if_else = try ifElse(context, i) },
        .cond => |c| return .{ .cond = try cond(context, c) },
        .call => |c| return try call(context, c),
        else => |k| std.debug.panic("\nUnsupported expression {}", .{k}),
    }
}

fn alloc(allocator: Allocator, expr: Expression) !*Expression {
    const result = try allocator.create(Expression);
    result.* = expr;
    return result;
}

fn expressionAlloc(context: Context, expr: ast.Expression) !*Expression {
    return try alloc(context.allocator, try expression(context, expr));
}

pub fn infer(module: *Module, name: Interned) !void {
    var work_queue = WorkQueue.init(module.allocator);
    try work_queue.append(name);
    while (work_queue.items.len != 0) {
        const current = work_queue.pop();
        if (module.untyped.fetchRemove(current)) |entry| {
            var scopes = try Scopes.init(module.allocator, &work_queue, module.scope);
            const context = Context{
                .allocator = module.allocator,
                .builtins = module.builtins,
                .constraints = module.constraints,
                .scopes = &scopes,
            };
            const expr = try expression(context, entry.value);
            try module.typed.putNoClobber(current, expr);
        }
    }
}
