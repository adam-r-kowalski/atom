const std = @import("std");
const Allocator = std.mem.Allocator;

const types = @import("types.zig");
const Substitution = types.Substitution;
const Ast = types.Ast;
const MonoType = types.MonoType;
const Typed = types.Typed;
const Symbol = types.Symbol;
const Int = types.Int;
const Float = types.Float;
const Bool = types.Bool;
const Function = types.Function;
const Expression = types.Expression;
const If = types.If;
const Cond = types.Cond;
const BinaryOp = types.BinaryOp;
const Define = types.Define;
const Call = types.Call;
const Intrinsic = types.Intrinsic;
const Block = types.Block;

fn monotype(allocator: Allocator, s: Substitution, m: MonoType) !MonoType {
    switch (m) {
        .i32 => return .i32,
        .i64 => return .i64,
        .f32 => return .f32,
        .f64 => return .f64,
        .str => return .str,
        .bool => return .bool,
        .void => return .void,
        .function => |f| {
            const mapped = try allocator.alloc(MonoType, f.len);
            for (f, mapped) |t, *mapped_t| mapped_t.* = try monotype(allocator, s, t);
            return .{ .function = mapped };
        },
        .typevar => |t| {
            if (s.get(t)) |mono| return mono;
            return m;
        },
    }
}

fn symbol(allocator: Allocator, s: Substitution, sym: Symbol) !Symbol {
    return Symbol{
        .value = sym.value,
        .span = sym.span,
        .type = try monotype(allocator, s, sym.type),
    };
}

fn int(allocator: Allocator, s: Substitution, i: Int) !Int {
    return Int{
        .value = i.value,
        .span = i.span,
        .type = try monotype(allocator, s, i.type),
    };
}

fn float(allocator: Allocator, s: Substitution, f: Float) !Float {
    return Float{
        .value = f.value,
        .span = f.span,
        .type = try monotype(allocator, s, f.type),
    };
}

fn ifElse(allocator: Allocator, s: Substitution, i: If) !If {
    return If{
        .condition = try expressionAlloc(allocator, s, i.condition.*),
        .then = try block(allocator, s, i.then),
        .else_ = try block(allocator, s, i.else_),
        .span = i.span,
        .type = try monotype(allocator, s, i.type),
    };
}

fn cond(allocator: Allocator, s: Substitution, c: Cond) !Cond {
    const conditions = try allocator.alloc(Expression, c.conditions.len);
    const thens = try allocator.alloc(Block, c.thens.len);
    for (c.conditions, c.thens, conditions, thens) |unapplied_c, unapplied_t, *applied_c, *applied_t| {
        applied_c.* = try expression(allocator, s, unapplied_c);
        applied_t.* = try block(allocator, s, unapplied_t);
    }
    return Cond{
        .conditions = conditions,
        .thens = thens,
        .else_ = try block(allocator, s, c.else_),
        .span = c.span,
        .type = try monotype(allocator, s, c.type),
    };
}

fn binaryOp(allocator: Allocator, s: Substitution, b: BinaryOp) !BinaryOp {
    return BinaryOp{
        .kind = b.kind,
        .left = try expressionAlloc(allocator, s, b.left.*),
        .right = try expressionAlloc(allocator, s, b.right.*),
        .span = b.span,
        .type = try monotype(allocator, s, b.type),
    };
}

fn define(allocator: Allocator, s: Substitution, d: Define) !Define {
    return Define{
        .name = try symbol(allocator, s, d.name),
        .value = try expressionAlloc(allocator, s, d.value.*),
        .span = d.span,
        .type = try monotype(allocator, s, d.type),
    };
}

fn call(allocator: Allocator, s: Substitution, c: Call) !Call {
    const arguments = try allocator.alloc(Expression, c.arguments.len);
    for (c.arguments, arguments) |unapplied, *applied| {
        applied.* = try expression(allocator, s, unapplied);
    }
    return Call{
        .function = try expressionAlloc(allocator, s, c.function.*),
        .arguments = arguments,
        .span = c.span,
        .type = try monotype(allocator, s, c.type),
    };
}

fn intrinsic(allocator: Allocator, s: Substitution, i: Intrinsic) !Intrinsic {
    const arguments = try allocator.alloc(Expression, i.arguments.len);
    for (i.arguments, arguments) |unapplied, *applied| {
        applied.* = try expression(allocator, s, unapplied);
    }
    return Intrinsic{
        .function = i.function,
        .arguments = arguments,
        .span = i.span,
        .type = try monotype(allocator, s, i.type),
    };
}

fn function(allocator: Allocator, s: Substitution, f: Function) !Function {
    const parameters = try allocator.alloc(Symbol, f.parameters.len);
    for (f.parameters, parameters) |unapplied, *applied| {
        applied.* = try symbol(allocator, s, unapplied);
    }
    return Function{
        .parameters = parameters,
        .return_type = try monotype(allocator, s, f.return_type),
        .body = try block(allocator, s, f.body),
        .span = f.span,
        .type = try monotype(allocator, s, f.type),
    };
}

fn block(allocator: Allocator, s: Substitution, b: Block) !Block {
    const expressions = try allocator.alloc(Expression, b.expressions.len);
    for (b.expressions, expressions) |unapplied, *applied| {
        applied.* = try expression(allocator, s, unapplied);
    }
    return Block{
        .expressions = expressions,
        .span = b.span,
        .type = try monotype(allocator, s, b.type),
    };
}

fn expression(allocator: Allocator, s: Substitution, e: Expression) error{OutOfMemory}!Expression {
    switch (e) {
        .symbol => |sym| return .{ .symbol = try symbol(allocator, s, sym) },
        .int => |i| return .{ .int = try int(allocator, s, i) },
        .float => |f| return .{ .float = try float(allocator, s, f) },
        .bool => |b| return .{ .bool = b },
        .string => |str| return .{ .string = str },
        .if_else => |i| return .{ .if_else = try ifElse(allocator, s, i) },
        .cond => |c| return .{ .cond = try cond(allocator, s, c) },
        .binary_op => |b| return .{ .binary_op = try binaryOp(allocator, s, b) },
        .define => |d| return .{ .define = try define(allocator, s, d) },
        .call => |c| return .{ .call = try call(allocator, s, c) },
        .intrinsic => |i| return .{ .intrinsic = try intrinsic(allocator, s, i) },
        .function => |f| return .{ .function = try function(allocator, s, f) },
        .block => |b| return .{ .block = try block(allocator, s, b) },
        .foreign_import => |f| return .{ .foreign_import = f },
        .convert => |c| return .{ .convert = c },
        else => |k| std.debug.panic("\nUnsupported expression {}", .{k}),
    }
}

fn expressionAlloc(allocator: Allocator, s: Substitution, e: Expression) !*const Expression {
    const expr = try allocator.create(Expression);
    expr.* = try expression(allocator, s, e);
    return expr;
}

pub fn apply(allocator: Allocator, s: Substitution, m: Ast) !Ast {
    var typed = Typed.init(allocator);
    var iterator = m.typed.iterator();
    while (iterator.next()) |entry| {
        if (m.typed.get(entry.key_ptr.*)) |t| {
            const value = try expression(allocator, s, t);
            try typed.putNoClobber(entry.key_ptr.*, value);
        }
    }
    return Ast{
        .allocator = m.allocator,
        .constraints = m.constraints,
        .next_type_var = m.next_type_var,
        .builtins = m.builtins,
        .order = m.order,
        .untyped = m.untyped,
        .typed = typed,
        .scope = m.scope,
        .intern = m.intern,
    };
}
