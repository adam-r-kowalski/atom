const std = @import("std");
const Allocator = std.mem.Allocator;
const Map = std.AutoHashMap;
const List = std.ArrayList;

const Builtins = @import("../builtins.zig").Builtins;
const interner = @import("../interner.zig");
const Interned = interner.Interned;
const Intern = interner.Intern;
const parser = @import("../parser.zig");
const Span = parser.Span;
const BinaryOpKind = parser.BinaryOpKind;
const UntypedExpression = parser.Expression;

pub const TypeVar = u64;

pub const MonoType = union(enum) {
    void,
    i32,
    i64,
    f32,
    f64,
    bool,
    str,
    typevar: TypeVar,
    function: []const MonoType,
};

pub const Int = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Float = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Symbol = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Bool = struct {
    value: bool,
    span: Span,
    type: MonoType,
};

pub const String = struct {
    value: Interned,
    span: Span,
    type: MonoType,
};

pub const Define = struct {
    name: Symbol,
    value: *const Expression,
    span: Span,
    type: MonoType,
};

pub const Block = struct {
    expressions: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Function = struct {
    parameters: []const Symbol,
    return_type: MonoType,
    body: Block,
    span: Span,
    type: MonoType,
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *const Expression,
    right: *const Expression,
    span: Span,
    type: MonoType,
};

pub const If = struct {
    condition: *const Expression,
    then: Block,
    else_: Block,
    span: Span,
    type: MonoType,
};

pub const Cond = struct {
    conditions: []const Expression,
    thens: []const Block,
    else_: Block,
    span: Span,
    type: MonoType,
};

pub const Call = struct {
    function: *const Expression,
    arguments: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Intrinsic = struct {
    function: Interned,
    arguments: []const Expression,
    span: Span,
    type: MonoType,
};

pub const Group = struct {
    expressions: []const Expression,
    span: Span,
    type: MonoType,
};

pub const ForeignImport = struct {
    module: Interned,
    name: Interned,
    span: Span,
    type: MonoType,
};

pub const Convert = struct {
    value: *const Expression,
    span: Span,
    type: MonoType,
};

pub const Expression = union(enum) {
    int: Int,
    float: Float,
    symbol: Symbol,
    bool: Bool,
    string: String,
    define: Define,
    function: Function,
    binary_op: BinaryOp,
    group: Group,
    block: Block,
    if_else: If,
    cond: Cond,
    call: Call,
    intrinsic: Intrinsic,
    foreign_import: ForeignImport,
    convert: Convert,

    pub fn typeOf(self: Expression) MonoType {
        return switch (self) {
            .int => |i| i.type,
            .float => |f| f.type,
            .symbol => |s| s.type,
            .bool => |b| b.type,
            .string => |s| s.type,
            .define => |d| d.type,
            .function => |f| f.type,
            .binary_op => |b| b.type,
            .group => |g| g.type,
            .block => |b| b.type,
            .if_else => |i| i.type,
            .cond => |c| c.type,
            .call => |c| c.type,
            .intrinsic => |i| i.type,
            .foreign_import => |f| f.type,
            .convert => |c| c.type,
        };
    }
};

pub const Untyped = Map(Interned, UntypedExpression);
pub const Typed = Map(Interned, Expression);

pub const Ast = struct {
    allocator: Allocator,
    constraints: *Constraints,
    next_type_var: *TypeVar,
    builtins: Builtins,
    order: []const Interned,
    untyped: Untyped,
    typed: Typed,
    scope: Scope,
    intern: *Intern,

    pub fn init(allocator: Allocator, constraints: *Constraints, next_type_var: *TypeVar, builtins: Builtins, ast: parser.Ast) !Ast {
        var order = List(Interned).init(allocator);
        var untyped = Untyped.init(allocator);
        var typed = Typed.init(allocator);
        var scope = Scope.init(allocator);
        for (ast.expressions) |top_level| {
            switch (top_level) {
                .define => |d| {
                    const name = d.name.value;
                    try order.append(name);
                    try untyped.putNoClobber(name, top_level);
                    const monotype = try topLevelType(allocator, builtins, d.value.*);
                    try scope.put(name, monotype);
                },
                else => |k| std.debug.panic("\nInvalid top level expression {}", .{k}),
            }
        }
        return Ast{
            .allocator = allocator,
            .constraints = constraints,
            .next_type_var = next_type_var,
            .builtins = builtins,
            .order = try order.toOwnedSlice(),
            .untyped = untyped,
            .typed = typed,
            .scope = scope,
            .intern = ast.intern,
        };
    }

    pub fn infer(self: *Ast, name: []const u8) !void {
        const interned = try self.intern.store(name);
        var work_queue = WorkQueue.init(self.allocator);
        try work_queue.append(interned);
        while (work_queue.items.len != 0) {
            const current = work_queue.pop();
            if (self.untyped.fetchRemove(current)) |entry| {
                var scopes = Scopes.init(self.allocator);
                try scopes.append(self.scope);
                const context = Context{
                    .allocator = self.allocator,
                    .work_queue = &work_queue,
                    .builtins = self.builtins,
                    .constraints = self.constraints,
                    .scopes = &scopes,
                    .next_type_var = self.next_type_var,
                };
                const expr = try expression(context, entry.value);
                try self.typed.putNoClobber(current, expr);
            }
        }
    }
};

pub const Scope = Map(Interned, MonoType);

pub const Scopes = List(Scope);

pub const Substitution = Map(TypeVar, MonoType);

pub const Equal = struct {
    left: MonoType,
    right: MonoType,
};

pub const Constraints = struct {
    equal: List(Equal),

    pub fn init(allocator: Allocator) Constraints {
        return Constraints{
            .equal = List(Equal).init(allocator),
        };
    }

    pub fn solve(self: Constraints, allocator: Allocator) !Substitution {
        var substitution = Substitution.init(allocator);
        for (self.equal.items) |e| try equal(&substitution, e);
        var max_attemps: u64 = 3;
        while (simplify(&substitution) > 0 and max_attemps != 0) : (max_attemps -= 1) {}
        return substitution;
    }
};

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

fn topLevelFunction(allocator: Allocator, builtins: Builtins, f: parser.Function) !MonoType {
    const len = f.parameters.len;
    const function_type = try allocator.alloc(MonoType, len + 1);
    for (f.parameters, function_type[0..len]) |p, *t|
        t.* = try expressionToMonoType(allocator, builtins, p.type);
    function_type[len] = try expressionToMonoType(allocator, builtins, f.return_type.*);
    return MonoType{ .function = function_type };
}

fn topLevelCall(allocator: Allocator, builtins: Builtins, c: parser.Call) !MonoType {
    switch (c.function.*) {
        .symbol => |s| {
            if (s.value == builtins.foreign_import) {
                if (c.arguments.len != 3) std.debug.panic("foreign_import takes 3 arguments", .{});
                return try expressionToMonoType(allocator, builtins, c.arguments[2]);
            }
        },
        else => |k| std.debug.panic("\nInvalid top level call function {}", .{k}),
    }
    std.debug.panic("\nInvalid top level call {}", .{c.function});
}

fn topLevelType(allocator: Allocator, builtins: Builtins, e: parser.Expression) !MonoType {
    return switch (e) {
        .function => |f| try topLevelFunction(allocator, builtins, f),
        .call => |c| try topLevelCall(allocator, builtins, c),
        else => |k| std.debug.panic("\nInvalid top level value {}", .{k}),
    };
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

fn cond(context: Context, c: parser.Cond) !Cond {
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

fn explicitTypeOrVar(allocator: Allocator, builtins: Builtins, next_type_var: *TypeVar, e: ?*const parser.Expression) !MonoType {
    return if (e) |t| try expressionToMonoType(allocator, builtins, t.*) else freshTypeVar(next_type_var);
}

fn define(context: Context, d: parser.Define) !Define {
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
            .type = arguments[0].typeOf(),
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
    const monotype = if (len == 0) .void else expressions[len - 1].typeOf();
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

fn set(substitution: *Substitution, t: TypeVar, m: MonoType) !void {
    const result = try substitution.getOrPut(t);
    if (result.found_existing) {
        if (std.meta.eql(result.value_ptr.*, m)) return;
        switch (m) {
            .typevar => |t1| try set(substitution, t1, result.value_ptr.*),
            else => switch (result.value_ptr.*) {
                .typevar => |t1| try set(substitution, t1, m),
                else => std.debug.panic("\nType mismatch: {} != {}\n", .{ result.value_ptr.*, m }),
            },
        }
    }
    result.value_ptr.* = m;
}

fn equal(substitution: *Substitution, e: Equal) !void {
    const left_tag = std.meta.activeTag(e.left);
    const right_tag = std.meta.activeTag(e.right);
    if (left_tag == .typevar)
        return try set(substitution, e.left.typevar, e.right);
    if (right_tag == .typevar)
        return try set(substitution, e.right.typevar, e.left);
    if (left_tag == .function and right_tag == .function) {
        if (e.left.function.len != e.right.function.len)
            std.debug.panic("\nFunction arity mismatch: {} != {}\n", .{
                e.left.function.len,
                e.right.function.len,
            });
        for (e.left.function, 0..) |left, i| {
            const right = e.right.function[i];
            try equal(substitution, Equal{ .left = left, .right = right });
        }
    }
    if (left_tag == right_tag)
        return;
    std.debug.panic("\nUnsupported type in equal: {} {}\n", .{ e.left, e.right });
}

fn simplify(substitution: *Substitution) u64 {
    var count: u64 = 0;
    var iterator = substitution.iterator();
    while (iterator.next()) |entry| {
        switch (entry.value_ptr.*) {
            .typevar => |t| {
                if (substitution.get(t)) |v| {
                    entry.value_ptr.* = v;
                    count += 1;
                }
            },
            else => {},
        }
    }
    return count;
}
