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
const Arm = typed_ast.Arm;
const Branch = typed_ast.Branch;
const Expression = typed_ast.Expression;
const Block = typed_ast.Block;
const Define = typed_ast.Define;
const Function = typed_ast.Function;
const Module = typed_ast.Module;
const Span = @import("span.zig").Span;
const ast = @import("ast.zig");

const Context = struct {
    allocator: Allocator,
    builtins: Builtins,
    constraints: *Constraints,
    scopes: *Scopes,
};

fn symbol(scopes: Scopes, s: ast.Symbol) !Symbol {
    return Symbol{
        .value = s.value,
        .span = s.span,
        .type = try scopes.find(s),
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

fn string(context: Context, s: ast.String) !String {
    const element_type = try context.allocator.create(MonoType);
    element_type.* = .u8;
    return String{
        .value = s.value,
        .span = s.span,
        .type = .{ .array = .{ .size = null, .element_type = element_type } },
    };
}

fn boolean(b: ast.Bool) Bool {
    return Bool{
        .value = b.value,
        .span = b.span,
        .type = .bool,
    };
}

fn branch(context: Context, b: ast.Branch) !Branch {
    const arms = try context.allocator.alloc(Arm, b.arms.len);
    const result_type = context.constraints.freshTypeVar();
    for (arms, b.arms) |*typed_arm, untyped_arm| {
        const condition = try expression(context, untyped_arm.condition);
        const then = try block(context, untyped_arm.then);
        typed_arm.* = Arm{ .condition = condition, .then = then };
        try context.constraints.equal.appendSlice(&[_]Equal{
            .{
                .left = .{ .type = condition.typeOf(), .span = condition.span() },
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
    return Branch{
        .arms = arms,
        .else_ = else_,
        .type = result_type,
        .span = b.span,
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
            try context.constraints.equal.append(.{
                .left = .{ .type = left.typeOf(), .span = b.left.span() },
                .right = .{ .type = right.typeOf(), .span = b.right.span() },
            });
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
            const left_typed_span = .{ .type = left.typeOf(), .span = b.left.span() };
            try context.constraints.equal.append(.{
                .left = left_typed_span,
                .right = .{ .type = right.typeOf(), .span = b.right.span() },
            });
            const tvar = context.constraints.freshTypeVar();
            try context.constraints.equal.append(.{
                .left = left_typed_span,
                .right = .{ .type = tvar, .span = null },
            });
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

fn define(context: Context, d: ast.Define) !Define {
    const value = try expressionAlloc(context, d.value.*);
    var monotype = value.typeOf();
    if (d.type) |t| {
        const annotated_type = try expressionToMonoType(context.allocator, context.builtins, t.*);
        try context.constraints.equal.append(.{
            .left = .{ .type = annotated_type, .span = t.span() },
            .right = .{ .type = monotype, .span = d.value.span() },
        });
        monotype = annotated_type;
    }
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

fn callForeignExport(context: Context, c: ast.Call) !Expression {
    if (c.arguments.len != 2) std.debug.panic("foreign_export takes 2 arguments", .{});
    return Expression{
        .foreign_export = .{
            .name = c.arguments[0].string.value,
            .value = try expressionAlloc(context, c.arguments[1]),
            .span = c.span,
            .type = .void,
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
            if (s.value.eql(context.builtins.foreign_export)) return try callForeignExport(context, c);
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
                .left = .{ .type = f.type, .span = f.span },
                .right = .{ .type = .{ .function = function_type }, .span = null },
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
    try context.constraints.equal.append(.{
        .left = .{ .type = return_type, .span = f.return_type.span() },
        .right = .{ .type = body.type, .span = body.span },
    });
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

fn expression(context: Context, e: ast.Expression) error{ OutOfMemory, CompileError }!Expression {
    switch (e) {
        .int => |i| return .{ .int = int(context, i) },
        .float => |f| return .{ .float = float(context, f) },
        .string => |s| return .{ .string = try string(context, s) },
        .symbol => |s| return .{ .symbol = try symbol(context.scopes.*, s) },
        .bool => |b| return .{ .bool = boolean(b) },
        .define => |d| return .{ .define = try define(context, d) },
        .function => |f| return .{ .function = try function(context, f) },
        .binary_op => |b| return try binaryOp(context, b),
        .block => |b| return .{ .block = try block(context, b) },
        .branch => |b| return .{ .branch = try branch(context, b) },
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
            var scopes = try Scopes.init(module.allocator, module.scope, &work_queue, module.compile_errors);
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
