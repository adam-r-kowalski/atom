const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoArrayHashMap;

const Interned = @import("../interner.zig").Interned;
const parser = @import("../parser.zig");
const Span = parser.Span;
const types = @import("types.zig");
const Int = types.Int;
const Float = types.Float;
const Bool = types.Bool;
const String = types.String;
const Symbol = types.Symbol;
const Ast = types.Ast;
const Untyped = types.Untyped;
const Typed = types.Typed;
const Scope = types.Scope;
const Scopes = types.Scopes;
const TopLevel = types.TopLevel;
const Function = types.Function;
const MonoType = types.MonoType;
const BinaryOp = types.BinaryOp;
const Block = types.Block;
const ForeignImport = types.ForeignImport;
const TypeVar = types.TypeVar;
const Expression = types.Expression;
const Constraints = types.Constraints;
const If = types.If;
const Cond = types.Cond;
const Define = types.Define;
const Call = types.Call;
const Equal = types.Equal;
const Builtins = @import("../builtins.zig").Builtins;
const typeOf = @import("type_of.zig").typeOf;

fn expressionToMonoType(allocator: Allocator, builtins: Builtins, e: parser.Expression) !MonoType {
    switch (e) {
        .symbol => |s| {
            if (s.value == builtins.i32) return .i32;
            if (s.value == builtins.i64) return .i64;
            if (s.value == builtins.f32) return .f32;
            if (s.value == builtins.f64) return .f64;
            if (s.value == builtins.bool) return .bool;
            if (s.value == builtins.str) return .str;
            if (s.value == builtins.void) return .void;
            std.debug.panic("\nCannot convert symbol {} to mono type", .{s});
        },
        .prototype => |p| {
            const len = p.parameters.len;
            const function_type = try allocator.alloc(MonoType, len + 1);
            for (p.parameters, function_type[0..len]) |param, *t|
                t.* = try expressionToMonoType(allocator, builtins, param.type);
            function_type[len] = try expressionToMonoType(allocator, builtins, p.return_type.*);
            return MonoType{ .function = function_type };
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

fn returnType(builtins: Builtins, next_type_var: *TypeVar, f: parser.Function) MonoType {
    return if (f.return_type) |t| expressionToMonoType(t.*, builtins) else freshTypeVar(next_type_var);
}

fn symbol(scopes: Scopes, work_queue: *WorkQueue, s: parser.Symbol) !Symbol {
    return Symbol{
        .value = s.value,
        .span = s.span,
        .type = try findInScope(scopes, work_queue, s.value),
    };
}

fn int(i: parser.Int, next_type_var: *TypeVar) Int {
    return Int{
        .value = i.value,
        .span = i.span,
        .type = freshTypeVar(next_type_var),
    };
}

fn float(f: parser.Float, next_type_var: *TypeVar) Float {
    return Float{
        .value = f.value,
        .span = f.span,
        .type = freshTypeVar(next_type_var),
    };
}

fn string(s: parser.String) String {
    return String{
        .value = s.value,
        .span = s.span,
        .type = .str,
    };
}

fn boolean(b: parser.Bool) Bool {
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

fn ifElse(context: Context, i: parser.If) !If {
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

fn cond(context: Context, c: parser.Cond) !Cond {
    const conditions = try context.allocator.alloc(Expression, c.conditions.len);
    const thens = try context.allocator.alloc(Block, c.thens.len);
    const type_ = freshTypeVar(context.next_type_var);
    for (conditions, thens, c.conditions, c.thens) |*typed_c, *typed_t, untyped_c, untyped_t| {
        typed_c.* = try expression(context, untyped_c);
        typed_t.* = try block(context, untyped_t);
        try context.constraints.equal.appendSlice(&[_]Equal{
            .{ .left = typeOf(typed_c.*), .right = .bool },
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

fn dotCall(context: Context, b: parser.BinaryOp) !Expression {
    switch (b.right.*) {
        .call => |c| {
            const arguments = try context.allocator.alloc(parser.Expression, c.arguments.len + 1);
            arguments[0] = b.left.*;
            @memcpy(arguments[1..], c.arguments);
            const new_call = parser.Call{
                .function = c.function,
                .arguments = arguments,
                .span = b.span,
            };
            return try call(context, new_call);
        },
        else => |k| std.debug.panic("Expected call after dot, got {}", .{k}),
    }
}

fn binaryOp(context: Context, b: parser.BinaryOp) !Expression {
    switch (b.kind) {
        .dot => return dotCall(context, b),
        .equal, .greater, .less => {
            const left = try expressionAlloc(context, b.left.*);
            const right = try expressionAlloc(context, b.right.*);
            const left_type = typeOf(left.*);
            try context.constraints.equal.append(.{ .left = left_type, .right = typeOf(right.*) });
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
            const left_type = typeOf(left.*);
            try context.constraints.equal.append(.{ .left = left_type, .right = typeOf(right.*) });
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

fn explicitTypeOrVar(allocator: Allocator, builtins: Builtins, next_type_var: *TypeVar, e: ?*const parser.Expression) !MonoType {
    return if (e) |t| try expressionToMonoType(allocator, builtins, t.*) else freshTypeVar(next_type_var);
}

fn define(context: Context, d: parser.Define) !Define {
    const value = try expressionAlloc(context, d.value.*);
    const type_ = try explicitTypeOrVar(context.allocator, context.builtins, context.next_type_var, d.type);
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

fn callForeignImport(context: Context, c: parser.Call) !Expression {
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

fn callConvert(context: Context, c: parser.Call) !Expression {
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

fn callSqrt(context: Context, c: parser.Call) !Expression {
    if (c.arguments.len != 1) std.debug.panic("sqrt takes 1 arguments", .{});
    const arguments = try context.allocator.alloc(Expression, 1);
    arguments[0] = try expression(context, c.arguments[0]);
    return Expression{
        .intrinsic = .{
            .function = context.builtins.sqrt,
            .arguments = arguments,
            .span = c.span,
            .type = typeOf(arguments[0]),
        },
    };
}

fn call(context: Context, c: parser.Call) !Expression {
    switch (c.function.*) {
        .symbol => |s| {
            const len = c.arguments.len;
            const function_type = try context.allocator.alloc(MonoType, len + 1);
            if (s.value == context.builtins.foreign_import) return try callForeignImport(context, c);
            if (s.value == context.builtins.convert) return try callConvert(context, c);
            if (s.value == context.builtins.sqrt) return try callSqrt(context, c);
            const f = try symbol(context.scopes.*, context.work_queue, s);
            const arguments = try context.allocator.alloc(Expression, len);
            for (c.arguments, arguments, function_type[0..len]) |untyped_arg, *typed_arg, *t| {
                typed_arg.* = try expression(context, untyped_arg);
                t.* = typeOf(typed_arg.*);
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

fn function(context: Context, f: parser.Function) !Function {
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

fn block(context: Context, b: parser.Block) !Block {
    const len = b.expressions.len;
    const expressions = try context.allocator.alloc(Expression, len);
    for (b.expressions, expressions) |untyped_e, *typed_e|
        typed_e.* = try expression(context, untyped_e);
    const monotype = if (len == 0) .void else typeOf(expressions[len - 1]);
    return Block{
        .expressions = expressions,
        .span = b.span,
        .type = monotype,
    };
}

fn expression(context: Context, e: parser.Expression) error{OutOfMemory}!Expression {
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

fn alloc(allocator: Allocator, expr: Expression) !*const Expression {
    const result = try allocator.create(Expression);
    result.* = expr;
    return result;
}

fn expressionAlloc(context: Context, expr: parser.Expression) !*const Expression {
    return try alloc(context.allocator, try expression(context, expr));
}

pub fn infer(allocator: Allocator, constraints: *Constraints, m: *Ast, builtins: Builtins, next_type_var: *TypeVar, name: Interned) !void {
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
