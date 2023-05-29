const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const types = @import("types.zig");
const Interned = @import("interner.zig").Interned;
const Builtins = @import("builtins.zig").Builtins;
const constraints = @import("constraints.zig");
const Constraints = constraints.Constraints;
const Equal = constraints.Equal;
const substitution = @import("substitution.zig");
const MonoType = substitution.MonoType;
const TypeVar = substitution.TypeVar;
const WorkQueue = List(Interned);
const Scope = types.typed_ast.Scope;
const Scopes = types.typed_ast.Scopes;
const expressionToMonoType = types.typed_ast.expressionToMonoType;
const Symbol = types.typed_ast.Symbol;
const Int = types.typed_ast.Int;
const Float = types.typed_ast.Float;
const String = types.typed_ast.String;
const Bool = types.typed_ast.Bool;
const If = types.typed_ast.If;
const Cond = types.typed_ast.Cond;
const Expression = types.typed_ast.Expression;
const Block = types.typed_ast.Block;
const Define = types.typed_ast.Define;
const Function = types.typed_ast.Function;
const Module = types.typed_ast.Module;
const Span = types.typed_ast.Span;
const ast = @import("ast.zig");

const Context = struct {
    allocator: Allocator,
    work_queue: *WorkQueue,
    builtins: Builtins,
    constraints: *Constraints,
    scopes: *Scopes,
    next_type_var: *TypeVar,
};

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

fn returnType(builtins: Builtins, next_type_var: *TypeVar, f: ast.Function) MonoType {
    return if (f.return_type) |t| expressionToMonoType(t.*, builtins) else freshTypeVar(next_type_var);
}

fn symbol(scopes: Scopes, work_queue: *WorkQueue, s: ast.Symbol) !Symbol {
    return Symbol{
        .value = s.value,
        .span = s.span,
        .type = try findInScope(scopes, work_queue, s.value),
    };
}

fn int(i: ast.Int, next_type_var: *TypeVar) Int {
    return Int{
        .value = i.value,
        .span = i.span,
        .type = freshTypeVar(next_type_var),
    };
}

fn float(f: ast.Float, next_type_var: *TypeVar) Float {
    return Float{
        .value = f.value,
        .span = f.span,
        .type = freshTypeVar(next_type_var),
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
    const type_ = freshTypeVar(context.next_type_var);
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
    const type_ = freshTypeVar(context.next_type_var);
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
            const tvar = freshTypeVar(context.next_type_var);
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

fn explicitTypeOrVar(allocator: Allocator, builtins: Builtins, next_type_var: *TypeVar, e: ?*const ast.Expression) !MonoType {
    return if (e) |t| try expressionToMonoType(allocator, builtins, t.*) else freshTypeVar(next_type_var);
}

fn define(context: Context, d: ast.Define) !Define {
    const value = try expressionAlloc(context, d.value.*);
    const type_ = try explicitTypeOrVar(context.allocator, context.builtins, context.next_type_var, d.type);
    try context.constraints.equal.append(.{ .left = value.typeOf(), .right = type_ });
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
            const f = try symbol(context.scopes.*, context.work_queue, s);
            const arguments = try context.allocator.alloc(Expression, len);
            for (c.arguments, arguments, function_type[0..len]) |untyped_arg, *typed_arg, *t| {
                typed_arg.* = try expression(context, untyped_arg);
                t.* = typed_arg.typeOf();
            }
            const return_type = freshTypeVar(context.next_type_var);
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
    try pushScope(context.scopes);
    defer popScope(context.scopes);
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
        try putInScope(context.scopes, name_symbol, p_type);
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
        .int => |i| return .{ .int = int(i, context.next_type_var) },
        .float => |f| return .{ .float = float(f, context.next_type_var) },
        .string => |s| return .{ .string = string(s) },
        .symbol => |s| return .{ .symbol = try symbol(context.scopes.*, context.work_queue, s) },
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

pub fn infer(module: *Module, name: []const u8) !void {
    const interned = try module.intern.store(name);
    var work_queue = WorkQueue.init(module.allocator);
    try work_queue.append(interned);
    while (work_queue.items.len != 0) {
        const current = work_queue.pop();
        if (module.untyped.fetchRemove(current)) |entry| {
            var scopes = Scopes.init(module.allocator);
            try scopes.append(module.scope);
            const context = Context{
                .allocator = module.allocator,
                .work_queue = &work_queue,
                .builtins = module.builtins,
                .constraints = module.constraints,
                .scopes = &scopes,
                .next_type_var = module.next_type_var,
            };
            const expr = try expression(context, entry.value);
            try module.typed.putNoClobber(current, expr);
        }
    }
}
