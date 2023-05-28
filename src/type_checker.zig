const std = @import("std");
const Allocator = std.mem.Allocator;
const Map = std.AutoHashMap;
const List = std.ArrayList;

const Builtins = @import("builtins.zig").Builtins;
const Indent = @import("indent.zig").Indent;
const interner = @import("interner.zig");
const Interned = interner.Interned;
const Intern = interner.Intern;
const parser = @import("parser.zig");
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
    function: []MonoType,

    fn apply(self: *MonoType, s: Substitution) void {
        switch (self.*) {
            .function => |f| for (f) |*t| t.apply(s),
            .typevar => |t| {
                if (s.get(t)) |mono| self.* = mono;
            },
            else => return,
        }
    }

    fn toString(self: MonoType, writer: anytype) !void {
        switch (self) {
            .i32 => try writer.writeAll("i32"),
            .i64 => try writer.writeAll("i64"),
            .f32 => try writer.writeAll("f32"),
            .f64 => try writer.writeAll("f64"),
            .str => try writer.writeAll("str"),
            .bool => try writer.writeAll("bool"),
            .void => try writer.writeAll("void"),
            .typevar => |t| try writer.print("${}", .{t}),
            .function => |f| {
                try writer.writeAll("fn(");
                for (f, 0..) |a, i| {
                    if (i == f.len - 1) {
                        try writer.writeAll(") ");
                    } else if (i > 0) {
                        try writer.writeAll(", ");
                    }
                    try a.toString(writer);
                }
            },
        }
    }
};

pub const Int = struct {
    value: Interned,
    span: Span,
    type: MonoType,

    fn apply(self: *Int, s: Substitution) void {
        self.type.apply(s);
    }

    fn toString(self: Int, writer: anytype, intern: Intern) !void {
        const value = intern.lookup(self.value);
        try writer.print("int{{ value = {s}, type = ", .{value});
        try self.type.toString(writer);
        try writer.writeAll(" }");
    }
};

pub const Float = struct {
    value: Interned,
    span: Span,
    type: MonoType,

    fn apply(self: *Float, s: Substitution) void {
        self.type.apply(s);
    }

    fn toString(self: Float, writer: anytype, intern: Intern) !void {
        const value = intern.lookup(self.value);
        try writer.print("float{{ value = {s}, type = ", .{value});
        try self.type.toString(writer);
        try writer.writeAll(" }");
    }
};

pub const Symbol = struct {
    value: Interned,
    span: Span,
    type: MonoType,

    fn apply(self: *Symbol, s: Substitution) void {
        self.type.apply(s);
    }

    fn toString(self: Symbol, writer: anytype, intern: Intern) !void {
        const value = intern.lookup(self.value);
        try writer.print("symbol{{ value = {s}, type = ", .{value});
        try self.type.toString(writer);
        try writer.writeAll(" }");
    }
};

pub const Bool = struct {
    value: bool,
    span: Span,
    type: MonoType,

    fn toString(self: Bool, writer: anytype) !void {
        try writer.print("bool{{ value = {}, type = ", .{self.value});
        try self.type.toString(writer);
        try writer.writeAll(" }");
    }
};

pub const String = struct {
    value: Interned,
    span: Span,
    type: MonoType,

    fn toString(self: String, writer: anytype, intern: Intern) !void {
        const value = intern.lookup(self.value);
        try writer.print("string{{ value = {s}, type = ", .{value});
        try self.type.toString(writer);
        try writer.writeAll(" }");
    }
};

pub const Define = struct {
    name: Symbol,
    value: *Expression,
    span: Span,
    type: MonoType,

    fn apply(self: *Define, s: Substitution) void {
        self.name.apply(s);
        self.value.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Define, writer: anytype, intern: Intern, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("define =");
        try indent.increment().toString(writer);
        try writer.writeAll("name = ");
        try self.name.toString(writer, intern);
        try indent.increment().toString(writer);
        try writer.writeAll("type = ");
        try self.type.toString(writer);
        try indent.increment().toString(writer);
        try writer.writeAll("value = ");
        try self.value.toString(writer, intern, indent.increment().increment());
    }
};

pub const Block = struct {
    expressions: []Expression,
    span: Span,
    type: MonoType,

    fn apply(self: *Block, s: Substitution) void {
        for (self.expressions) |*e| e.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Block, writer: anytype, intern: Intern, indent: Indent) !void {
        for (self.expressions) |expr| try expr.toString(writer, intern, indent);
    }
};

pub const Function = struct {
    parameters: []Symbol,
    return_type: MonoType,
    body: Block,
    span: Span,
    type: MonoType,

    fn apply(self: *Function, s: Substitution) void {
        for (self.parameters) |*p| p.apply(s);
        self.return_type.apply(s);
        self.body.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Function, writer: anytype, intern: Intern, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("function =");
        if (self.parameters.len != 0) {
            try indent.increment().toString(writer);
            try writer.print("parameters =", .{});
        }
        for (self.parameters) |p| {
            try indent.increment().increment().toString(writer);
            try p.toString(writer, intern);
        }
        try indent.increment().toString(writer);
        try writer.print("return_type = ", .{});
        try self.return_type.toString(writer);
        try indent.increment().toString(writer);
        try writer.print("body = ", .{});
        try self.body.toString(writer, intern, indent.increment().increment());
    }
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *Expression,
    right: *Expression,
    span: Span,
    type: MonoType,

    fn apply(self: *BinaryOp, s: Substitution) void {
        self.left.apply(s);
        self.right.apply(s);
        self.type.apply(s);
    }

    fn toString(self: BinaryOp, writer: anytype, intern: Intern, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("binary_op =");
        try indent.increment().toString(writer);
        try writer.writeAll("kind = ");
        try self.kind.toString(writer);
        try indent.increment().toString(writer);
        try writer.writeAll("left = ");
        try self.left.toString(writer, intern, indent.increment().increment());
        try indent.increment().toString(writer);
        try writer.writeAll("right = ");
        try self.right.toString(writer, intern, indent.increment().increment());
        try indent.increment().toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
};

pub const If = struct {
    condition: *Expression,
    then: Block,
    else_: Block,
    span: Span,
    type: MonoType,

    fn apply(self: *If, s: Substitution) void {
        self.condition.apply(s);
        self.then.apply(s);
        self.else_.apply(s);
        self.type.apply(s);
    }

    fn toString(self: If, writer: anytype, intern: Intern, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("if =");
        try indent.increment().toString(writer);
        try writer.writeAll("condition = ");
        try self.condition.toString(writer, intern, indent.increment().increment());
        try indent.increment().toString(writer);
        try writer.writeAll("then = ");
        try self.then.toString(writer, intern, indent.increment().increment());
        try indent.increment().toString(writer);
        try writer.writeAll("else = ");
        try self.else_.toString(writer, intern, indent.increment().increment());
        try indent.increment().toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
};

pub const Cond = struct {
    conditions: []Expression,
    thens: []Block,
    else_: Block,
    span: Span,
    type: MonoType,

    fn apply(self: *Cond, s: Substitution) void {
        for (self.conditions, self.thens) |*c, *t| {
            c.apply(s);
            t.apply(s);
        }
        self.else_.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Cond, writer: anytype, intern: Intern, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("cond =");
        for (self.conditions, self.thens) |c, t| {
            try indent.increment().toString(writer);
            try writer.writeAll("condition = ");
            try c.toString(writer, intern, indent.increment().increment());
            try indent.increment().toString(writer);
            try writer.writeAll("then = ");
            try t.toString(writer, intern, indent.increment().increment());
        }
        try indent.increment().toString(writer);
        try writer.writeAll("else = ");
        try self.else_.toString(writer, intern, indent.increment().increment());
        try indent.increment().toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
};

pub const Call = struct {
    function: *Expression,
    arguments: []Expression,
    span: Span,
    type: MonoType,

    fn apply(self: *Call, s: Substitution) void {
        self.function.apply(s);
        for (self.arguments) |*a| a.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Call, writer: anytype, intern: Intern, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("call =");
        try indent.increment().toString(writer);
        try self.function.toString(writer, intern, indent.increment().increment());
        try indent.increment().toString(writer);
        try writer.writeAll("arguments =");
        for (self.arguments) |a| {
            try indent.increment().increment().toString(writer);
            try a.toString(writer, intern, indent.increment().increment().increment());
        }
        try indent.increment().toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
};

pub const Intrinsic = struct {
    function: Interned,
    arguments: []Expression,
    span: Span,
    type: MonoType,

    fn apply(self: *Intrinsic, s: Substitution) void {
        for (self.arguments) |*a| a.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Intrinsic, writer: anytype, intern: Intern, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("intrinsic =");
        try indent.increment().toString(writer);
        try writer.writeAll(intern.lookup(self.function));
        try indent.increment().toString(writer);
        try writer.writeAll("arguments =");
        for (self.arguments) |a| {
            try indent.increment().increment().toString(writer);
            try a.toString(writer, intern, indent.increment().increment().increment());
        }
        try indent.increment().toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
};

pub const Group = struct {
    expressions: []Expression,
    span: Span,
    type: MonoType,

    fn apply(self: *Group, s: Substitution) void {
        for (self.expressions) |*e| e.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Group, writer: anytype, intern: Intern, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("group =");
        try indent.increment().toString(writer);
        try writer.writeAll("expressions =");
        for (self.expressions) |expr| {
            try indent.increment().increment().toString(writer);
            try expr.toString(writer, intern, indent.increment().increment());
        }
        try indent.increment().toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
};

pub const ForeignImport = struct {
    module: Interned,
    name: Interned,
    span: Span,
    type: MonoType,

    fn toString(self: ForeignImport, writer: anytype, intern: Intern, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("foreign_import =");
        try indent.increment().toString(writer);
        const module = intern.lookup(self.module);
        try writer.print("module = {s}", .{module});
        try indent.increment().toString(writer);
        const name = intern.lookup(self.name);
        try writer.print("name = {s}", .{name});
        try indent.increment().toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
};

pub const Convert = struct {
    value: *Expression,
    span: Span,
    type: MonoType,

    fn toString(self: Convert, writer: anytype, intern: Intern, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("convert =");
        try indent.increment().toString(writer);
        try writer.print("value = ", .{});
        try self.value.toString(writer, intern, indent.increment());
        try indent.increment().toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
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

    fn apply(self: *Expression, s: Substitution) void {
        switch (self.*) {
            .symbol => |*sym| sym.apply(s),
            .int => |*i| i.apply(s),
            .float => |*f| f.apply(s),
            .bool => return,
            .string => return,
            .if_else => |*i| i.apply(s),
            .cond => |*c| c.apply(s),
            .binary_op => |*b| b.apply(s),
            .define => |*d| d.apply(s),
            .call => |*c| c.apply(s),
            .intrinsic => |*i| i.apply(s),
            .function => |*f| f.apply(s),
            .block => |*b| b.apply(s),
            .group => |*g| g.apply(s),
            .foreign_import => return,
            .convert => return,
        }
    }

    fn toString(self: Expression, writer: anytype, intern: Intern, indent: Indent) error{NoSpaceLeft}!void {
        switch (self) {
            .symbol => |s| try s.toString(writer, intern),
            .int => |i| try i.toString(writer, intern),
            .float => |f| try f.toString(writer, intern),
            .string => |s| try s.toString(writer, intern),
            .bool => |b| try b.toString(writer),
            .if_else => |i| try i.toString(writer, intern, indent),
            .cond => |c| try c.toString(writer, intern, indent),
            .binary_op => |b| try b.toString(writer, intern, indent),
            .call => |c| try c.toString(writer, intern, indent),
            .intrinsic => |i| try i.toString(writer, intern, indent),
            .define => |d| try d.toString(writer, intern, indent),
            .function => |f| try f.toString(writer, intern, indent),
            .block => |b| try b.toString(writer, intern, indent),
            .group => |g| try g.toString(writer, intern, indent),
            .foreign_import => |f| try f.toString(writer, intern, indent),
            .convert => |c| try c.toString(writer, intern, indent),
        }
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

    pub fn apply(self: *Ast, s: Substitution) void {
        var iterator = self.typed.valueIterator();
        while (iterator.next()) |value_ptr| value_ptr.apply(s);
    }

    pub fn format(
        self: Ast,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        for (self.order, 0..) |name, i| {
            if (self.typed.get(name)) |e| {
                if (i > 0) try writer.writeAll("\n\n");
                e.toString(writer, self.intern.*, Indent{ .value = 0 }) catch unreachable;
            }
        }
    }
};

pub const Scope = Map(Interned, MonoType);

pub const Scopes = List(Scope);

pub const Substitution = struct {
    map: Map(TypeVar, MonoType),

    fn init(allocator: Allocator) Substitution {
        return Substitution{
            .map = Map(TypeVar, MonoType).init(allocator),
        };
    }

    fn set(self: *Substitution, t: TypeVar, m: MonoType) !void {
        const result = try self.map.getOrPut(t);
        if (result.found_existing) {
            if (std.meta.eql(result.value_ptr.*, m)) return;
            switch (m) {
                .typevar => |t1| try self.set(t1, result.value_ptr.*),
                else => switch (result.value_ptr.*) {
                    .typevar => |t1| try self.set(t1, m),
                    else => std.debug.panic("\nType mismatch: {} != {}\n", .{ result.value_ptr.*, m }),
                },
            }
        }
        result.value_ptr.* = m;
    }

    fn get(self: Substitution, t: TypeVar) ?MonoType {
        return self.map.get(t);
    }

    fn equal(self: *Substitution, e: Equal) !void {
        const left_tag = std.meta.activeTag(e.left);
        const right_tag = std.meta.activeTag(e.right);
        if (left_tag == .typevar)
            return try self.set(e.left.typevar, e.right);
        if (right_tag == .typevar)
            return try self.set(e.right.typevar, e.left);
        if (left_tag == .function and right_tag == .function) {
            if (e.left.function.len != e.right.function.len)
                std.debug.panic("\nFunction arity mismatch: {} != {}\n", .{
                    e.left.function.len,
                    e.right.function.len,
                });
            for (e.left.function, 0..) |left, i| {
                const right = e.right.function[i];
                try self.equal(Equal{ .left = left, .right = right });
            }
        }
        if (left_tag == right_tag)
            return;
        std.debug.panic("\nUnsupported type in equal: {} {}\n", .{ e.left, e.right });
    }

    fn simplify(self: *Substitution) u64 {
        var count: u64 = 0;
        var iterator = self.map.iterator();
        while (iterator.next()) |entry| {
            switch (entry.value_ptr.*) {
                .typevar => |t| {
                    if (self.map.get(t)) |v| {
                        entry.value_ptr.* = v;
                        count += 1;
                    }
                },
                else => {},
            }
        }
        return count;
    }

    fn toString(self: Substitution, writer: anytype) !void {
        try writer.writeAll("\n\n=== Substitution ===");
        var iterator = self.map.iterator();
        while (iterator.next()) |t| {
            try writer.print("\n${} = ", .{t.key_ptr.*});
            try t.value_ptr.toString(writer);
        }
    }
};

pub const Equal = struct {
    left: MonoType,
    right: MonoType,

    fn toString(self: Equal, writer: anytype) !void {
        try writer.print("equal = ", .{});
        try (Indent{ .value = 1 }).toString(writer);
        try writer.print("left = ", .{});
        try self.left.toString(writer);
        try (Indent{ .value = 1 }).toString(writer);
        try writer.print("right = ", .{});
        try self.right.toString(writer);
    }
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
        for (self.equal.items) |e| try substitution.equal(e);
        var max_attemps: u64 = 3;
        while (substitution.simplify() > 0 and max_attemps != 0) : (max_attemps -= 1) {}
        return substitution;
    }

    fn toString(self: Constraints, writer: anytype) !void {
        try writer.writeAll("\n\n=== Constraints ===");
        for (self.equal.items) |e| {
            try writer.writeAll("\n");
            try e.toString(writer);
        }
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

fn alloc(allocator: Allocator, expr: Expression) !*Expression {
    const result = try allocator.create(Expression);
    result.* = expr;
    return result;
}

fn expressionAlloc(context: Context, expr: parser.Expression) !*Expression {
    return try alloc(context.allocator, try expression(context, expr));
}
