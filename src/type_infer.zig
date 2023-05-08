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
    bool,
    unit,
    type,
    int_literal: Interned,
    bool_literal: bool,
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
    try context.typed_ast.type.putNoClobber(expr, type_);
    const kind = context.typed_ast.ast.kind.items[expr];
    switch (kind) {
        .symbol => {
            const s = context.typed_ast.ast.symbol.items[context.typed_ast.ast.index.items[expr]];
            if (s == context.builtins.i32) return .i32;
            if (s == context.builtins.bool) return .bool;
            std.debug.panic("\nCannot type symbol {}", .{s});
        },
        else => std.debug.panic("\nCannot type expression {}", .{kind}),
    }
}

fn block(context: Context, exprs: List(Expression)) !Type {
    std.debug.assert(exprs.items.len > 0);
    var last: Type = 0;
    for (exprs.items) |expr| last = try infer(context, expr);
    return last;
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
        try context.typed_ast.type.putNoClobber(p.name, type_);
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
    const last = try block(context, f.body);
    try context.constraints.equal.append(.{ .left = last, .right = return_type });
    const index = context.types.function.items.len;
    try context.types.function.append(.{
        .parameters = parameters,
        .return_type = return_type,
    });
    const type_ = context.types.kind.items.len;
    try context.types.kind.append(.{ .function = index });
    try context.types.span.putNoClobber(type_, full_span);
    try context.typed_ast.type.putNoClobber(expr, type_);
    return type_;
}

fn define(context: Context, expr: Expression) !Type {
    const d = context.typed_ast.ast.define.items[context.typed_ast.ast.index.items[expr]];
    const body_type = try block(context, d.body);
    const annotated_type = blk: {
        var type_ = context.types.kind.items.len;
        if (d.type) |t| {
            const kind = try typeExpression(context, t);
            type_ = context.types.kind.items.len;
            try context.types.kind.append(kind);
            const span = context.typed_ast.ast.span.items[t];
            try context.types.span.putNoClobber(type_, span);
        } else {
            const kind = freshTypeVar(context.types);
            try context.types.kind.append(kind);
        }
        break :blk type_;
    };
    try context.typed_ast.type.putNoClobber(d.name, annotated_type);
    try context.constraints.equal.append(.{ .left = annotated_type, .right = body_type });
    const type_ = context.types.kind.items.len;
    try context.types.kind.append(.unit);
    const span = context.typed_ast.ast.span.items[expr];
    try context.types.span.putNoClobber(type_, span);
    try context.typed_ast.type.putNoClobber(expr, type_);
    const s = context.typed_ast.ast.symbol.items[context.typed_ast.ast.index.items[d.name]];
    // TODO: need to generalize this to allow for polymorphic functions
    try putInScope(context.scopes, s, annotated_type);
    return type_;
}

fn binaryOp(context: Context, expr: Expression) !Type {
    const b = context.typed_ast.ast.binary_op.items[context.typed_ast.ast.index.items[expr]];
    const left = try infer(context, b.left);
    const right = try infer(context, b.right);
    const type_ = context.types.kind.items.len;
    try context.types.kind.append(freshTypeVar(context.types));
    try context.types.span.putNoClobber(type_, context.typed_ast.ast.span.items[expr]);
    try context.typed_ast.type.putNoClobber(expr, type_);
    try context.constraints.equal.append(.{ .left = left, .right = type_ });
    try context.constraints.equal.append(.{ .left = type_, .right = right });
    return type_;
}

fn if_(context: Context, expr: Expression) !Type {
    const i = context.typed_ast.ast.if_.items[context.typed_ast.ast.index.items[expr]];
    const condition = try infer(context, i.condition);
    const bool_type = context.types.kind.items.len;
    try context.types.kind.append(.bool);
    try context.constraints.equal.append(.{ .left = condition, .right = bool_type });
    const then = try block(context, i.then);
    const else_ = try block(context, i.else_);
    const type_ = context.types.kind.items.len;
    try context.types.kind.append(freshTypeVar(context.types));
    try context.types.span.putNoClobber(type_, context.typed_ast.ast.span.items[expr]);
    try context.typed_ast.type.putNoClobber(expr, type_);
    try context.constraints.equal.append(.{ .left = then, .right = type_ });
    try context.constraints.equal.append(.{ .left = type_, .right = else_ });
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
            try context.typed_ast.type.putNoClobber(expr, type_);
            return type_;
        }
    }
    std.debug.panic("\nCannot find symbol {}", .{s});
}

fn int(context: Context, expr: Expression) !Type {
    const i = context.typed_ast.ast.int.items[context.typed_ast.ast.index.items[expr]];
    const type_ = context.types.kind.items.len;
    try context.types.kind.append(.{ .int_literal = i });
    try context.types.span.putNoClobber(type_, context.typed_ast.ast.span.items[expr]);
    try context.typed_ast.type.putNoClobber(expr, type_);
    return type_;
}

fn boolean(context: Context, expr: Expression) !Type {
    const b = context.typed_ast.ast.bool.items[context.typed_ast.ast.index.items[expr]];
    const type_ = context.types.kind.items.len;
    try context.types.kind.append(.{ .bool_literal = b });
    try context.types.span.putNoClobber(type_, context.typed_ast.ast.span.items[expr]);
    try context.typed_ast.type.putNoClobber(expr, type_);
    return type_;
}

fn infer(context: Context, expr: Expression) error{OutOfMemory}!Type {
    const kind = context.typed_ast.ast.kind.items[expr];
    switch (kind) {
        .function => return try function(context, expr),
        .define => return try define(context, expr),
        .symbol => return try symbol(context, expr),
        .binary_op => return try binaryOp(context, expr),
        .if_ => return try if_(context, expr),
        .int => return try int(context, expr),
        .bool => return try boolean(context, expr),
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

fn mapTypeVar(substitution: *Substitution, types: Types, typevar: TypeVar, type_: Type) !void {
    const result = try substitution.getOrPut(typevar);
    if (!result.found_existing) {
        result.value_ptr.* = type_;
        return;
    }
    const result_kind = types.kind.items[result.value_ptr.*];
    const type_kind = types.kind.items[type_];
    switch (result_kind) {
        .typevar => |t| {
            switch (type_kind) {
                .typevar => |t2| {
                    if (t == t2) return;
                    result.value_ptr.* = type_;
                    try mapTypeVar(substitution, types, t, type_);
                    return;
                },
                else => {
                    result.value_ptr.* = type_;
                    try mapTypeVar(substitution, types, t, type_);
                    return;
                },
            }
        },
        else => {
            switch (type_kind) {
                .typevar => |t| try mapTypeVar(substitution, types, t, result.value_ptr.*),
                else => {
                    if (std.meta.eql(type_kind, result_kind)) return;
                    std.debug.panic("\nCannot unify types {} and {}", .{
                        type_kind,
                        result_kind,
                    });
                },
            }
        },
    }
}

fn equal(substitution: *Substitution, types: Types, e: Equal) !void {
    const left_kind = types.kind.items[e.left];
    const right_kind = types.kind.items[e.right];
    if (left_kind == .typevar) {
        if (right_kind == .typevar) {
            if (left_kind.typevar == right_kind.typevar) return;
            return try mapTypeVar(substitution, types, left_kind.typevar, e.right);
        }
        return try mapTypeVar(substitution, types, left_kind.typevar, e.right);
    }
    if (right_kind == .typevar) {
        if (left_kind == .typevar) {
            if (left_kind.typevar == right_kind.typevar) return;
            return try mapTypeVar(substitution, types, right_kind.typevar, e.left);
        }
        return try mapTypeVar(substitution, types, right_kind.typevar, e.left);
    }
    if (left_kind == .function and right_kind == .function)
        std.debug.panic("\nCannot handle function types yet", .{});
    if (left_kind == .int_literal) switch (right_kind) {
        .int_literal, .i32 => return,
        else => std.debug.panic("\nCannot unify types {} and {}", .{ left_kind, right_kind }),
    };
    if (right_kind == .int_literal) switch (left_kind) {
        .int_literal, .i32 => return,
        else => std.debug.panic("\nCannot unify types {} and {}", .{ left_kind, right_kind }),
    };
    if (left_kind == .bool_literal) switch (right_kind) {
        .bool_literal, .bool => return,
        else => std.debug.panic("\nCannot unify types {} and {}", .{ left_kind, right_kind }),
    };
    if (right_kind == .bool_literal) switch (left_kind) {
        .bool_literal, .bool => return,
        else => std.debug.panic("\nCannot unify types {} and {}", .{ left_kind, right_kind }),
    };
    if (std.meta.eql(left_kind, right_kind)) return;
    std.debug.panic("\nCannot unify types {} and {}", .{ left_kind, right_kind });
}

pub fn solve(allocator: Allocator, types: Types, constraints: Constraints) !Substitution {
    var substitution = Substitution.init(allocator);
    for (constraints.equal.items) |e| try equal(&substitution, types, e);
    //TODO: there has to be a better way to approach this
    var i: u64 = 0;
    while (i < 3) : (i += 1) {
        var solved: u64 = 0;
        var iterator = substitution.iterator();
        while (iterator.next()) |entry| {
            switch (types.kind.items[entry.value_ptr.*]) {
                .typevar => |t| {
                    if (substitution.get(t)) |type_| {
                        entry.value_ptr.* = type_;
                        solved += 1;
                    }
                },
                else => {},
            }
        }
        if (solved == 0) return substitution;
    }
    std.debug.panic("\nCannot solve constraints", .{});
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

const Vars = Map(TypeVar, u8);

fn typeToString(writer: List(u8).Writer, intern: Intern, types: Types, type_: Type, vars: *Vars) !void {
    const kind = types.kind.items[type_];
    switch (kind) {
        .i32 => try writer.writeAll("i32"),
        .bool => try writer.writeAll("bool"),
        .typevar => |t| {
            const result = try vars.getOrPut(t);
            if (result.found_existing) {
                return try std.fmt.format(writer, "{c}", .{result.value_ptr.*});
            }
            result.value_ptr.* = 'A' + @intCast(u8, vars.count() - 1);
            try std.fmt.format(writer, "{c}", .{result.value_ptr.*});
        },
        .int_literal => |i| try writer.writeAll(interner.lookup(intern, i)),
        .bool_literal => |b| try writer.writeAll(if (b) "true" else "false"),
        else => std.debug.panic("\nCannot convert type {} to string", .{kind}),
    }
}

fn symbolToString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, expr: Expression) !void {
    const s = typed_ast.ast.symbol.items[typed_ast.ast.index.items[expr]];
    try writer.writeAll(interner.lookup(intern, s));
}

fn blockToString(allocator: Allocator, writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, exprs: List(Expression), indent: u64) !void {
    if (exprs.items.len == 1) {
        try writer.writeAll(" ");
        try expressionToString(allocator, writer, intern, typed_ast, types, exprs.items[0], indent);
        return;
    }
    for (exprs.items) |expr| {
        try indentToString(writer, indent);
        try expressionToString(allocator, writer, intern, typed_ast, types, expr, indent);
    }
}

fn functionToString(allocator: Allocator, writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) !void {
    var vars = Vars.init(allocator);
    defer vars.deinit();
    var list = List(u8).init(allocator);
    defer list.deinit();
    const sub_writer = list.writer();
    const f = typed_ast.ast.function.items[typed_ast.ast.index.items[expr]];
    const type_ = typed_ast.type.get(expr).?;
    const kind = types.kind.items[type_];
    const f_type = types.function.items[kind.function];
    try symbolToString(writer, intern, typed_ast, f.name);
    try sub_writer.writeAll("(");
    for (f.parameters.items) |p, i| {
        if (i > 0) try sub_writer.writeAll(", ");
        try symbolToString(sub_writer, intern, typed_ast, p.name);
        try sub_writer.writeAll(": ");
        try typeToString(sub_writer, intern, types, f_type.parameters.items[i], &vars);
    }
    try sub_writer.writeAll(") -> ");
    try typeToString(sub_writer, intern, types, f_type.return_type, &vars);
    try sub_writer.writeAll(" =");
    try blockToString(allocator, sub_writer, intern, typed_ast, types, f.body, indent + 1);
    const num_vars = vars.count();
    if (num_vars > 0) {
        try writer.writeAll("[");
        var i: usize = 0;
        while (i < num_vars) : (i += 1) {
            if (i > 0) try writer.writeAll(", ");
            try std.fmt.format(writer, "{c}", .{'A' + @intCast(u8, i)});
        }
        try writer.writeAll("]");
    }
    try writer.writeAll(list.items);
}

fn defineToString(allocator: Allocator, writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) !void {
    //TODO: this should come from the function
    var vars = Vars.init(allocator);
    defer vars.deinit();
    const d = typed_ast.ast.define.items[typed_ast.ast.index.items[expr]];
    try symbolToString(writer, intern, typed_ast, d.name);
    try writer.writeAll(": ");
    const name_type = typed_ast.type.get(d.name).?;
    try typeToString(writer, intern, types, name_type, &vars);
    try writer.writeAll(" =");
    try blockToString(allocator, writer, intern, typed_ast, types, d.body, indent + 1);
}

fn binaryOpToString(allocator: Allocator, writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) !void {
    const b = typed_ast.ast.binary_op.items[typed_ast.ast.index.items[expr]];
    try expressionToString(allocator, writer, intern, typed_ast, types, b.left, indent);
    try writer.writeAll(" ");
    switch (b.kind) {
        .add => try writer.writeAll("+"),
        .multiply => try writer.writeAll("*"),
        else => unreachable,
    }
    try writer.writeAll(" ");
    try expressionToString(allocator, writer, intern, typed_ast, types, b.right, indent);
}

fn ifToString(allocator: Allocator, writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) !void {
    const i = typed_ast.ast.if_.items[typed_ast.ast.index.items[expr]];
    try writer.writeAll("if ");
    try expressionToString(allocator, writer, intern, typed_ast, types, i.condition, indent);
    try writer.writeAll(" then");
    try blockToString(allocator, writer, intern, typed_ast, types, i.then, indent);
    try writer.writeAll(" else");
    try blockToString(allocator, writer, intern, typed_ast, types, i.else_, indent);
}

fn intToString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, expr: Expression) !void {
    const i = typed_ast.ast.int.items[typed_ast.ast.index.items[expr]];
    try writer.writeAll(interner.lookup(intern, i));
}

fn boolToString(writer: List(u8).Writer, typed_ast: TypedAst, expr: Expression) !void {
    const b = typed_ast.ast.bool.items[typed_ast.ast.index.items[expr]];
    try writer.writeAll(if (b) "true" else "false");
}

fn expressionToString(allocator: Allocator, writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) error{OutOfMemory}!void {
    switch (typed_ast.ast.kind.items[expr]) {
        .function => try functionToString(allocator, writer, intern, typed_ast, types, expr, indent),
        .define => try defineToString(allocator, writer, intern, typed_ast, types, expr, indent),
        .symbol => try symbolToString(writer, intern, typed_ast, expr),
        .binary_op => try binaryOpToString(allocator, writer, intern, typed_ast, types, expr, indent),
        .if_ => try ifToString(allocator, writer, intern, typed_ast, types, expr, indent),
        .int => try intToString(writer, intern, typed_ast, expr),
        .bool => try boolToString(writer, typed_ast, expr),
        else => unreachable,
    }
}

pub fn toString(allocator: Allocator, intern: Intern, typed_ast: TypedAst, types: Types) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    const indent: u64 = 0;
    for (typed_ast.ast.top_level.items) |expr| try expressionToString(allocator, writer, intern, typed_ast, types, expr, indent);
    return list.toOwnedSlice();
}

fn indentToString(writer: List(u8).Writer, indent: u64) !void {
    try writer.writeAll("\n");
    var i: u64 = 0;
    while (i < indent) {
        try writer.writeAll("    ");
        i += 1;
    }
}

fn symbolToVerboseString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) !void {
    try indentToString(writer, indent);
    const s = typed_ast.ast.symbol.items[typed_ast.ast.index.items[expr]];
    try writer.writeAll("symbol = ");
    try indentToString(writer, indent + 1);
    try writer.writeAll("name = ");
    try writer.writeAll(interner.lookup(intern, s));
    try indentToString(writer, indent + 1);
    try writer.writeAll("type = ");
    if (typed_ast.type.get(expr)) |t| {
        try typeToVerboseString(writer, types, t);
    } else {
        try writer.writeAll("unknown");
    }
}

fn typeToVerboseString(writer: List(u8).Writer, types: Types, type_: Type) !void {
    const kind = types.kind.items[type_];
    switch (kind) {
        .i32 => try writer.writeAll("i32"),
        .bool => try writer.writeAll("bool"),
        .unit => try writer.writeAll("unit"),
        .typevar => |i| try std.fmt.format(writer, "typevar {}", .{i}),
        .function => |i| {
            const f = types.function.items[i];
            try writer.writeAll("function (");
            for (f.parameters.items) |p, j| {
                if (j > 0) try writer.writeAll(", ");
                try typeToVerboseString(writer, types, p);
            }
            try writer.writeAll(") -> ");
            try typeToVerboseString(writer, types, f.return_type);
        },
        else => std.debug.panic("\nCannot convert type {} to string", .{kind}),
    }
}

fn blockToVerboseString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, exprs: List(Expression), indent: u64) !void {
    for (exprs.items) |expr| {
        try indentToString(writer, indent);
        try expressionToVerboseString(writer, intern, typed_ast, types, expr, indent);
    }
}

fn functionToVerboseString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) !void {
    const f = typed_ast.ast.function.items[typed_ast.ast.index.items[expr]];
    const f_type = typed_ast.type.get(expr).?;
    try writer.writeAll("function");
    try indentToString(writer, indent + 1);
    try writer.writeAll("name = ");
    try symbolToVerboseString(writer, intern, typed_ast, types, f.name, indent + 2);
    try indentToString(writer, indent + 1);
    try writer.writeAll("type = ");
    try typeToVerboseString(writer, types, f_type);
    try indentToString(writer, indent + 1);
    try writer.writeAll("parameters = ");
    for (f.parameters.items) |p| {
        try indentToString(writer, indent + 2);
        try writer.writeAll("parameter = ");
        try indentToString(writer, indent + 3);
        try writer.writeAll("name = ");
        try symbolToVerboseString(writer, intern, typed_ast, types, p.name, indent + 4);
        try indentToString(writer, indent + 3);
        try writer.writeAll("type = ");
        const p_type = typed_ast.type.get(p.name).?;
        try typeToVerboseString(writer, types, p_type);
    }
    try indentToString(writer, indent + 1);
    try writer.writeAll("body = ");
    try blockToVerboseString(writer, intern, typed_ast, types, f.body, indent + 2);
}

fn defineToVerboseString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) !void {
    const d = typed_ast.ast.define.items[typed_ast.ast.index.items[expr]];
    const d_type = typed_ast.type.get(d.name).?;
    try writer.writeAll("define");
    try indentToString(writer, indent + 1);
    try writer.writeAll("name = ");
    try symbolToVerboseString(writer, intern, typed_ast, types, d.name, indent + 2);
    try indentToString(writer, indent + 1);
    try writer.writeAll("type = ");
    try typeToVerboseString(writer, types, d_type);
    try indentToString(writer, indent + 1);
    try writer.writeAll("body = ");
    try blockToVerboseString(writer, intern, typed_ast, types, d.body, indent + 2);
}

fn binaryOpToVerboseString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) !void {
    const b = typed_ast.ast.binary_op.items[typed_ast.ast.index.items[expr]];
    try indentToString(writer, indent);
    try writer.writeAll("binary op");
    try indentToString(writer, indent + 1);
    try writer.writeAll("kind = ");
    switch (b.kind) {
        .add => try writer.writeAll("+"),
        .multiply => try writer.writeAll("*"),
        else => unreachable,
    }
    try indentToString(writer, indent + 1);
    try writer.writeAll("type = ");
    try typeToVerboseString(writer, types, typed_ast.type.get(expr).?);
    try indentToString(writer, indent + 1);
    try writer.writeAll("lhs = ");
    try expressionToVerboseString(writer, intern, typed_ast, types, b.left, indent + 2);
    try indentToString(writer, indent + 1);
    try writer.writeAll("rhs = ");
    try expressionToVerboseString(writer, intern, typed_ast, types, b.right, indent + 2);
}

fn ifToVerboseString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) !void {
    const i = typed_ast.ast.if_.items[typed_ast.ast.index.items[expr]];
    try indentToString(writer, indent);
    try writer.writeAll("if");
    try indentToString(writer, indent + 1);
    try writer.writeAll("condition = ");
    try expressionToVerboseString(writer, intern, typed_ast, types, i.condition, indent + 2);
    try indentToString(writer, indent + 1);
    try writer.writeAll("then = ");
    try blockToVerboseString(writer, intern, typed_ast, types, i.then, indent + 2);
    try indentToString(writer, indent + 1);
    try writer.writeAll("else = ");
    try blockToVerboseString(writer, intern, typed_ast, types, i.else_, indent + 2);
}

fn expressionToVerboseString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types, expr: Expression, indent: u64) error{OutOfMemory}!void {
    switch (typed_ast.ast.kind.items[expr]) {
        .function => try functionToVerboseString(writer, intern, typed_ast, types, expr, indent),
        .define => try defineToVerboseString(writer, intern, typed_ast, types, expr, indent),
        .binary_op => try binaryOpToVerboseString(writer, intern, typed_ast, types, expr, indent),
        .if_ => try ifToVerboseString(writer, intern, typed_ast, types, expr, indent),
        .symbol => try symbolToVerboseString(writer, intern, typed_ast, types, expr, indent),
        else => std.debug.panic("\nCannot convert expression {} to string", .{typed_ast.ast.kind.items[expr]}),
    }
}

pub fn toVerboseString(writer: List(u8).Writer, intern: Intern, typed_ast: TypedAst, types: Types) !void {
    const indent: u64 = 0;
    for (typed_ast.ast.top_level.items) |expr| try expressionToVerboseString(writer, intern, typed_ast, types, expr, indent);
}

pub fn substitutionsToVerboseString(writer: List(u8).Writer, substitution: Substitution, types: Types) !void {
    var iterator = substitution.iterator();
    while (iterator.next()) |entry| {
        try std.fmt.format(writer, "\n{} -> ", .{entry.key_ptr.*});
        try typeToVerboseString(writer, types, entry.value_ptr.*);
    }
}

pub fn constraintsToVerboseString(writer: List(u8).Writer, constraints: Constraints, types: Types) !void {
    for (constraints.equal.items) |c| {
        try writer.writeAll("\n");
        try typeToVerboseString(writer, types, c.left);
        try writer.writeAll(" == ");
        try typeToVerboseString(writer, types, c.right);
    }
}
