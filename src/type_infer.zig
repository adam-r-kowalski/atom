const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;
const Tuple = std.meta.Tuple;

fn Set(comptime T: type) type {
    return std.AutoHashMap(T, void);
}

const Builtins = @import("builtins.zig").Builtins;
const interner = @import("interner.zig");
const Interned = interner.Interned;
const tokenizer = @import("tokenizer.zig");
const Span = tokenizer.Span;
const parser = @import("parser.zig");
const Ast = parser.Ast;
const Expression = parser.Expression;
pub const TypeVar = u64;

const Type = union(enum) {
    i32,
    type,
    typevar: TypeVar,
};

const TypeAndSpan = struct {
    type: Type,
    span: Span,
};

const EqualConstraint = Tuple(&.{ TypeAndSpan, TypeAndSpan });

pub const Constraints = struct {
    equal: List(EqualConstraint),

    pub fn init(allocator: Allocator) Constraints {
        return Constraints{
            .equal = List(EqualConstraint).init(allocator),
        };
    }

    pub fn deinit(self: Constraints) void {
        self.equal.deinit();
    }
};

const Scope = Map(Interned, Expression);

const Scopes = List(Scope);

fn pushScope(scopes: *Scopes) !void {
    var scope = Scope.init(scopes.allocator);
    try scopes.append(scope);
}

pub const TypedAst = struct {
    ast: Ast,
    type: Map(Expression, Type),
    scope: Scope,

    pub fn init(allocator: Allocator, ast: Ast) !TypedAst {
        var scope = Scope.init(allocator);
        for (ast.top_level.items) |expr| {
            switch (ast.kind.items[expr]) {
                .define => try scope.put(ast.define.items[ast.index.items[expr]].name, expr),
                .function => try scope.put(ast.function.items[ast.index.items[expr]].name, expr),
                else => std.debug.panic("\nInvalid top level expression {}", .{ast.kind.items[expr]}),
            }
        }
        return TypedAst{
            .ast = ast,
            .type = Map(Expression, Type).init(allocator),
            .scope = scope,
        };
    }

    pub fn deinit(self: *TypedAst) void {
        self.type.deinit();
        self.scope.deinit();
    }
};

const Context = struct {
    allocator: Allocator,
    constraints: *Constraints,
    typed_ast: *TypedAst,
    scopes: *Scopes,
    next_typevar: *TypeVar,
    builtins: Builtins,
};

fn freshTypeVar(next_typevar: *TypeVar) Type {
    const typevar = next_typevar.*;
    next_typevar.* += 1;
    return .{ .typevar = typevar };
}

fn typeExpression(context: Context, expr: Expression) !Type {
    try context.typed_ast.type.put(expr, .type);
    const kind = context.typed_ast.ast.kind.items[expr];
    switch (kind) {
        .symbol => {
            const symbol = context.typed_ast.ast.symbol.items[context.typed_ast.ast.index.items[expr]];
            if (symbol == context.builtins.i32) return .i32;
            std.debug.panic("\nCannot type symbol {}", .{symbol});
        },
        else => std.debug.panic("\nCannot type expression {}", .{kind}),
    }
}

fn function(context: Context, expr: Expression) !Type {
    try pushScope(context.scopes);
    var f = context.typed_ast.ast.function.items[context.typed_ast.ast.index.items[expr]];
    for (f.parameters.items) |p| {
        const type_ = if (p.type) |t| try typeExpression(context, t) else freshTypeVar(context.next_typevar);
        try context.typed_ast.type.put(p.name, type_);
    }
    return .i32;
}

fn infer(context: Context, expr: Expression) !Type {
    const kind = context.typed_ast.ast.kind.items[expr];
    switch (kind) {
        .function => return try function(context, expr),
        else => std.debug.panic("\nCannot infer type of expression {}", .{kind}),
    }
}

pub fn constrain(allocator: Allocator, constraints: *Constraints, typed_ast: *TypedAst, builtins: Builtins, next_type_var: *TypeVar, name: Interned) !void {
    var scopes = Scopes.init(allocator);
    const context = Context{
        .allocator = allocator,
        .constraints = constraints,
        .typed_ast = typed_ast,
        .scopes = &scopes,
        .next_typevar = next_type_var,
        .builtins = builtins,
    };
    if (typed_ast.scope.get(name)) |expr| {
        if (!typed_ast.type.contains(expr)) _ = try infer(context, expr);
    } else {
        std.debug.panic("\nUndefined name {}", .{name});
    }
}
