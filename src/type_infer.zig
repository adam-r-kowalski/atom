const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;

fn Set(comptime T: type) type {
    return std.AutoHashMap(T, void);
}

const Builtins = @import("builtins.zig").Builtins;
const interner = @import("interner.zig");
const Interned = interner.Interned;
const Intern = interner.Intern;
const tokenizer = @import("tokenizer.zig");
const Span = tokenizer.Span;
const parser = @import("parser.zig");
const Ast = parser.Ast;
const Expression = parser.Expression;
pub const TypeVar = u64;

const Kind = union(enum) {
    i32,
    type,
    typevar: TypeVar,
    function: u64,
};

const Function = struct {
    parameters: List(Type),
    return_type: Type,
};

const Type = u64;

pub const Types = struct {
    kind: List(Kind),
    function: List(Function),
    span: Map(Type, Span),
    next_type_var: TypeVar,

    pub fn init(allocator: Allocator) Types {
        return Types{
            .kind = List(Kind).init(allocator),
            .function = List(Function).init(allocator),
            .span = Map(Type, Span).init(allocator),
            .next_type_var = 0,
        };
    }

    pub fn deinit(self: *Types) void {
        self.kind.deinit();
        self.span.deinit();
        for (self.function.items) |f| f.parameters.deinit();
        self.function.deinit();
    }
};

const Equal = struct {
    left: Type,
    right: Type,
};

pub const Constraints = struct {
    equal: List(Equal),

    pub fn init(allocator: Allocator) Constraints {
        return Constraints{
            .equal = List(Equal).init(allocator),
        };
    }

    pub fn deinit(self: Constraints) void {
        self.equal.deinit();
    }
};

const Scope = Map(Interned, Type);

const Scopes = List(Scope);

fn pushScope(scopes: *Scopes) !void {
    var scope = Scope.init(scopes.allocator);
    try scopes.append(scope);
}

fn popScope(scopes: *Scopes) void {
    var scope = scopes.pop();
    scope.deinit();
}

fn putInScope(scopes: *Scopes, name: Interned, type_: Type) !void {
    try scopes.items[scopes.items.len - 1].put(name, type_);
}

pub const TypedAst = struct {
    ast: Ast,
    type: Map(Expression, Type),
    scope: Scope,

    pub fn init(allocator: Allocator, ast: Ast) !TypedAst {
        var scope = Scope.init(allocator);
        for (ast.top_level.items) |expr| {
            switch (ast.kind.items[expr]) {
                .define => {
                    const s = ast.define.items[ast.index.items[expr]].name;
                    const name = ast.symbol.items[ast.index.items[s]];
                    try scope.put(name, expr);
                },
                .function => {
                    const s = ast.function.items[ast.index.items[expr]].name;
                    const name = ast.symbol.items[ast.index.items[s]];
                    try scope.put(name, expr);
                },
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
    types: *Types,
    scopes: *Scopes,
    builtins: Builtins,
};

fn freshTypeVar(types: *Types) Kind {
    const type_var = types.next_type_var;
    types.next_type_var += 1;
    return .{ .typevar = type_var };
}

fn typeExpression(context: Context, expr: Expression) !Kind {
    const type_ = context.types.kind.items.len;
    try context.types.kind.append(.type);
    try context.types.span.putNoClobber(type_, context.typed_ast.ast.span.items[expr]);
    try context.typed_ast.type.put(expr, type_);
    const kind = context.typed_ast.ast.kind.items[expr];
    switch (kind) {
        .symbol => {
            const s = context.typed_ast.ast.symbol.items[context.typed_ast.ast.index.items[expr]];
            if (s == context.builtins.i32) return .i32;
            std.debug.panic("\nCannot type symbol {}", .{s});
        },
        else => std.debug.panic("\nCannot type expression {}", .{kind}),
    }
}

// Need to put function name in scope before inferring body to allow for recursion
fn function(context: Context, expr: Expression) !Type {
    try pushScope(context.scopes);
    defer popScope(context.scopes);
    const f = context.typed_ast.ast.function.items[context.typed_ast.ast.index.items[expr]];
    var parameters = List(Type).init(context.allocator);
    var full_span = context.typed_ast.ast.span.items[f.name];
    for (f.parameters.items) |p| {
        var type_ = context.types.kind.items.len;
        var span = context.typed_ast.ast.span.items[p.name];
        if (p.type) |t| {
            const kind = try typeExpression(context, t);
            type_ = context.types.kind.items.len;
            try context.types.kind.append(kind);
            span.end = context.typed_ast.ast.span.items[t].end;
        } else {
            const kind = freshTypeVar(context.types);
            try context.types.kind.append(kind);
        }
        full_span.end = span.end;
        try context.types.span.putNoClobber(type_, span);
        try context.typed_ast.type.put(p.name, type_);
        try parameters.append(type_);
        const s = context.typed_ast.ast.symbol.items[context.typed_ast.ast.index.items[p.name]];
        try putInScope(context.scopes, s, type_);
    }
    const return_type = blk: {
        var type_ = context.types.kind.items.len;
        if (f.return_type) |t| {
            const kind = try typeExpression(context, t);
            type_ = context.types.kind.items.len;
            try context.types.kind.append(kind);
            const span = context.typed_ast.ast.span.items[t];
            full_span.end = span.end;
            try context.types.span.putNoClobber(type_, span);
        } else {
            const kind = freshTypeVar(context.types);
            try context.types.kind.append(kind);
        }
        break :blk type_;
    };
    var last: Type = 0;
    for (f.body.items) |b| last = try infer(context, b);
    try context.constraints.equal.append(.{ .left = last, .right = return_type });
    const index = context.types.function.items.len;
    try context.types.function.append(.{
        .parameters = parameters,
        .return_type = return_type,
    });
    const type_ = context.types.kind.items.len;
    try context.types.kind.append(.{ .function = index });
    try context.types.span.putNoClobber(type_, full_span);
    try context.typed_ast.type.put(expr, type_);
    return type_;
}

// TODO: need to instantiate the type if it's a forall
fn symbol(context: Context, expr: Expression) !Type {
    const s = context.typed_ast.ast.symbol.items[context.typed_ast.ast.index.items[expr]];
    var i: u64 = context.scopes.items.len;
    while (i > 0) : (i -= 1) {
        if (context.scopes.items[i - 1].get(s)) |t| {
            const type_ = context.types.kind.items.len;
            const kind = context.types.kind.items[t];
            try context.types.kind.append(kind);
            try context.types.span.putNoClobber(type_, context.typed_ast.ast.span.items[expr]);
            return type_;
        }
    }
    std.debug.panic("\nCannot find symbol {}", .{s});
}

fn infer(context: Context, expr: Expression) error{OutOfMemory}!Type {
    const kind = context.typed_ast.ast.kind.items[expr];
    switch (kind) {
        .function => return try function(context, expr),
        .symbol => return try symbol(context, expr),
        else => std.debug.panic("\nCannot infer type of expression {}", .{kind}),
    }
}

pub fn constrain(allocator: Allocator, constraints: *Constraints, typed_ast: *TypedAst, types: *Types, builtins: Builtins, name: Interned) !void {
    var scopes = Scopes.init(allocator);
    try scopes.append(typed_ast.scope);
    defer scopes.deinit();
    const context = Context{
        .allocator = allocator,
        .constraints = constraints,
        .typed_ast = typed_ast,
        .types = types,
        .scopes = &scopes,
        .builtins = builtins,
    };
    if (typed_ast.scope.get(name)) |expr| {
        if (!typed_ast.type.contains(expr)) {
            _ = try infer(context, expr);
        }
    } else {
        std.debug.panic("\nUndefined name {}", .{name});
    }
}

const Substitution = Map(TypeVar, Type);

fn equal(substitution: *Substitution, types: Types, e: Equal) !void {
    const left_kind = types.kind.items[e.left];
    const right_kind = types.kind.items[e.right];
    if (left_kind == .typevar) {
        if (right_kind == .typevar) {
            if (left_kind.typevar == right_kind.typevar) return;
            return try substitution.putNoClobber(left_kind.typevar, e.right);
        }
        return try substitution.putNoClobber(left_kind.typevar, e.right);
    }
    if (right_kind == .typevar) {
        if (left_kind == .typevar) {
            if (left_kind.typevar == right_kind.typevar) return;
            return try substitution.putNoClobber(right_kind.typevar, e.left);
        }
        return try substitution.putNoClobber(right_kind.typevar, e.left);
    }
    if (left_kind == .function and right_kind == .function) {
        std.debug.panic("\nCannot handle function types yet", .{});
    }
    if (std.meta.eql(left_kind, right_kind)) return;
    std.debug.panic("\nCannot unify types {} and {}", .{ left_kind, right_kind });
}

pub fn solve(allocator: Allocator, types: Types, constraints: Constraints) !Substitution {
    var substitution = Substitution.init(allocator);
    for (constraints.equal.items) |e| try equal(&substitution, types, e);
    return substitution;
}

pub fn apply(substitution: Substitution, types: *Types) void {
    for (types.kind.items) |*kind| {
        switch (kind.*) {
            .typevar => |typevar| {
                if (substitution.get(typevar)) |type_| {
                    kind.* = types.kind.items[type_];
                }
            },
            else => {},
        }
    }
}

fn typeToString(writer: List(u8).Writer, types: Types, type_: Type) !void {
    const kind = types.kind.items[type_];
    switch (kind) {
        .i32 => try writer.writeAll("i32"),
        else => unreachable,
    }
}

fn symbolToString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, expr: Expression) !void {
    const s = typed_ast.ast.symbol.items[typed_ast.ast.index.items[expr]];
    try writer.writeAll(interner.lookup(intern, s));
}

fn blockToString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, exprs: List(Expression), indent: u64) !void {
    std.debug.assert(exprs.items.len == 1);
    try expressionToString(writer, intern, typed_ast, types, exprs.items[0], indent);
}

fn functionToString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) !void {
    const f = typed_ast.ast.function.items[typed_ast.ast.index.items[expr]];
    const type_ = typed_ast.type.get(expr).?;
    const kind = types.kind.items[type_];
    const f_type = types.function.items[kind.function];
    try symbolToString(writer, intern, typed_ast, f.name);
    try writer.writeAll("(");
    for (f.parameters.items) |p, i| {
        try symbolToString(writer, intern, typed_ast, p.name);
        try writer.writeAll(": ");
        try typeToString(writer, types, f_type.parameters.items[i]);
    }
    try writer.writeAll(") -> ");
    try typeToString(writer, types, f_type.return_type);
    try writer.writeAll(" = ");
    try blockToString(writer, intern, typed_ast, types, f.body, indent);
}

fn expressionToString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) error{OutOfMemory}!void {
    switch (typed_ast.ast.kind.items[expr]) {
        .function => try functionToString(writer, intern, typed_ast, types, expr, indent),
        .symbol => try symbolToString(writer, intern, typed_ast, expr),
        else => unreachable,
    }
}

pub fn toString(allocator: Allocator, intern: Intern, typed_ast: TypedAst, types: Types) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    const indent: u64 = 0;
    for (typed_ast.ast.top_level.items) |expr| try expressionToString(writer, intern, typed_ast, types, expr, indent);
    return list.toOwnedSlice();
}
