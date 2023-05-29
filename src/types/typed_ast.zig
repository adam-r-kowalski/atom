const std = @import("std");
const Allocator = std.mem.Allocator;
const Map = std.AutoHashMap;
const List = std.ArrayList;

const Builtins = @import("../builtins.zig").Builtins;
const Indent = @import("../indent.zig").Indent;
const interner = @import("../interner.zig");
const Interned = interner.Interned;
const Intern = interner.Intern;
const untyped_ast = @import("../ast.zig");
pub const Span = untyped_ast.Span;
const BinaryOpKind = untyped_ast.BinaryOpKind;
const UntypedExpression = untyped_ast.Expression;
const substitution = @import("../substitution.zig");
const MonoType = substitution.MonoType;
const Substitution = substitution.Substitution;
const TypeVar = substitution.TypeVar;
const Constraints = @import("../constraints.zig").Constraints;

pub const Scope = Map(Interned, MonoType);

pub const Scopes = List(Scope);

pub const Int = struct {
    value: Interned,
    span: Span,
    type: MonoType,

    pub fn apply(self: *Int, s: Substitution) void {
        self.type.apply(s);
    }

    fn toString(self: Int, writer: anytype) !void {
        try writer.print("int{{ value = {}, type = ", .{self.value});
        try self.type.toString(writer);
        try writer.writeAll(" }");
    }
};

pub const Float = struct {
    value: Interned,
    span: Span,
    type: MonoType,

    pub fn apply(self: *Float, s: Substitution) void {
        self.type.apply(s);
    }

    fn toString(self: Float, writer: anytype) !void {
        try writer.print("float{{ value = {}, type = ", .{self.value});
        try self.type.toString(writer);
        try writer.writeAll(" }");
    }
};

pub const Symbol = struct {
    value: Interned,
    span: Span,
    type: MonoType,

    pub fn apply(self: *Symbol, s: Substitution) void {
        self.type.apply(s);
    }

    fn toString(self: Symbol, writer: anytype) !void {
        try writer.print("symbol{{ value = {}, type = ", .{self.value});
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

    fn toString(self: String, writer: anytype) !void {
        try writer.print("string{{ value = {}, type = ", .{self.value});
        try self.type.toString(writer);
        try writer.writeAll(" }");
    }
};

pub const Define = struct {
    name: Symbol,
    value: *Expression,
    span: Span,
    type: MonoType,

    pub fn apply(self: *Define, s: Substitution) void {
        self.name.apply(s);
        self.value.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Define, writer: anytype, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("define =");
        try indent.add(1).toString(writer);
        try writer.writeAll("name = ");
        try self.name.toString(writer);
        try indent.add(1).toString(writer);
        try writer.writeAll("type = ");
        try self.type.toString(writer);
        try indent.add(1).toString(writer);
        try writer.writeAll("value = ");
        try self.value.toString(writer, indent.add(2));
    }
};

pub const Block = struct {
    expressions: []Expression,
    span: Span,
    type: MonoType,

    pub fn apply(self: *Block, s: Substitution) void {
        for (self.expressions) |*e| e.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Block, writer: anytype, indent: Indent) !void {
        for (self.expressions) |expr| try expr.toString(writer, indent);
    }
};

pub const Function = struct {
    parameters: []Symbol,
    return_type: MonoType,
    body: Block,
    span: Span,
    type: MonoType,

    pub fn apply(self: *Function, s: Substitution) void {
        for (self.parameters) |*p| p.apply(s);
        self.return_type.apply(s);
        self.body.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Function, writer: anytype, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("function =");
        if (self.parameters.len != 0) {
            try indent.add(1).toString(writer);
            try writer.print("parameters =", .{});
        }
        for (self.parameters) |p| {
            try indent.add(2).toString(writer);
            try p.toString(writer);
        }
        try indent.add(1).toString(writer);
        try writer.print("return_type = ", .{});
        try self.return_type.toString(writer);
        try indent.add(1).toString(writer);
        try writer.print("body = ", .{});
        try self.body.toString(writer, indent.add(2));
    }
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *Expression,
    right: *Expression,
    span: Span,
    type: MonoType,

    pub fn apply(self: *BinaryOp, s: Substitution) void {
        self.left.apply(s);
        self.right.apply(s);
        self.type.apply(s);
    }

    fn toString(self: BinaryOp, writer: anytype, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("binary_op =");
        try indent.add(1).toString(writer);
        try writer.writeAll("kind = ");
        try self.kind.toString(writer);
        try indent.add(1).toString(writer);
        try writer.writeAll("left = ");
        try self.left.toString(writer, indent.add(2));
        try indent.add(1).toString(writer);
        try writer.writeAll("right = ");
        try self.right.toString(writer, indent.add(2));
        try indent.add(1).toString(writer);
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

    pub fn apply(self: *If, s: Substitution) void {
        self.condition.apply(s);
        self.then.apply(s);
        self.else_.apply(s);
        self.type.apply(s);
    }

    fn toString(self: If, writer: anytype, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("if =");
        try indent.add(1).toString(writer);
        try writer.writeAll("condition = ");
        try self.condition.toString(writer, indent.add(2));
        try indent.add(1).toString(writer);
        try writer.writeAll("then = ");
        try self.then.toString(writer, indent.add(2));
        try indent.add(1).toString(writer);
        try writer.writeAll("else = ");
        try self.else_.toString(writer, indent.add(2));
        try indent.add(1).toString(writer);
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

    pub fn apply(self: *Cond, s: Substitution) void {
        for (self.conditions, self.thens) |*c, *t| {
            c.apply(s);
            t.apply(s);
        }
        self.else_.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Cond, writer: anytype, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("cond =");
        for (self.conditions, self.thens) |c, t| {
            try indent.add(1).toString(writer);
            try writer.writeAll("condition = ");
            try c.toString(writer, indent.add(2));
            try indent.add(1).toString(writer);
            try writer.writeAll("then = ");
            try t.toString(writer, indent.add(2));
        }
        try indent.add(1).toString(writer);
        try writer.writeAll("else = ");
        try self.else_.toString(writer, indent.add(2));
        try indent.add(1).toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
};

pub const Call = struct {
    function: *Expression,
    arguments: []Expression,
    span: Span,
    type: MonoType,

    pub fn apply(self: *Call, s: Substitution) void {
        self.function.apply(s);
        for (self.arguments) |*a| a.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Call, writer: anytype, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("call =");
        try indent.add(1).toString(writer);
        try self.function.toString(writer, indent.add(2));
        try indent.add(1).toString(writer);
        try writer.writeAll("arguments =");
        for (self.arguments) |a| {
            try indent.add(2).toString(writer);
            try a.toString(writer, indent.add(3));
        }
        try indent.add(1).toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
};

pub const Intrinsic = struct {
    function: Interned,
    arguments: []Expression,
    span: Span,
    type: MonoType,

    pub fn apply(self: *Intrinsic, s: Substitution) void {
        for (self.arguments) |*a| a.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Intrinsic, writer: anytype, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("intrinsic =");
        try indent.add(1).toString(writer);
        try writer.print("{}", .{self.function});
        try indent.add(1).toString(writer);
        try writer.writeAll("arguments =");
        for (self.arguments) |a| {
            try indent.add(2).toString(writer);
            try a.toString(writer, indent.add(3));
        }
        try indent.add(1).toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
};

pub const Group = struct {
    expressions: []Expression,
    span: Span,
    type: MonoType,

    pub fn apply(self: *Group, s: Substitution) void {
        for (self.expressions) |*e| e.apply(s);
        self.type.apply(s);
    }

    fn toString(self: Group, writer: anytype, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("group =");
        try indent.add(1).toString(writer);
        try writer.writeAll("expressions =");
        for (self.expressions) |expr| {
            try indent.add(2).toString(writer);
            try expr.toString(writer, indent.add(2));
        }
        try indent.add(1).toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
};

pub const ForeignImport = struct {
    module: Interned,
    name: Interned,
    span: Span,
    type: MonoType,

    fn toString(self: ForeignImport, writer: anytype, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("foreign_import =");
        try indent.add(1).toString(writer);
        try writer.print("module = {}", .{self.module});
        try indent.add(1).toString(writer);
        try writer.print("name = {}", .{self.name});
        try indent.add(1).toString(writer);
        try writer.print("type = ", .{});
        try self.type.toString(writer);
    }
};

pub const Convert = struct {
    value: *Expression,
    span: Span,
    type: MonoType,

    fn toString(self: Convert, writer: anytype, indent: Indent) !void {
        try indent.toString(writer);
        try writer.writeAll("convert =");
        try indent.add(1).toString(writer);
        try writer.print("value = ", .{});
        try self.value.toString(writer, indent.add(1));
        try indent.add(1).toString(writer);
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

    pub fn apply(self: *Expression, s: Substitution) void {
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

    fn toString(self: Expression, writer: anytype, indent: Indent) error{NoSpaceLeft}!void {
        switch (self) {
            .symbol => |s| try s.toString(writer),
            .int => |i| try i.toString(writer),
            .float => |f| try f.toString(writer),
            .string => |s| try s.toString(writer),
            .bool => |b| try b.toString(writer),
            .if_else => |i| try i.toString(writer, indent),
            .cond => |c| try c.toString(writer, indent),
            .binary_op => |b| try b.toString(writer, indent),
            .call => |c| try c.toString(writer, indent),
            .intrinsic => |i| try i.toString(writer, indent),
            .define => |d| try d.toString(writer, indent),
            .function => |f| try f.toString(writer, indent),
            .block => |b| try b.toString(writer, indent),
            .group => |g| try g.toString(writer, indent),
            .foreign_import => |f| try f.toString(writer, indent),
            .convert => |c| try c.toString(writer, indent),
        }
    }
};

pub const Untyped = Map(Interned, UntypedExpression);
pub const Typed = Map(Interned, Expression);

pub const Module = struct {
    allocator: Allocator,
    constraints: *Constraints,
    next_type_var: *TypeVar,
    builtins: Builtins,
    order: []const Interned,
    untyped: Untyped,
    typed: Typed,
    scope: Scope,
    intern: *Intern,

    pub fn init(allocator: Allocator, constraints: *Constraints, next_type_var: *TypeVar, builtins: Builtins, ast: untyped_ast.Module) !Module {
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
        return Module{
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

    pub fn apply(self: *Module, s: Substitution) void {
        var iterator = self.typed.valueIterator();
        while (iterator.next()) |value_ptr| value_ptr.apply(s);
    }

    pub fn format(
        self: Module,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        for (self.order, 0..) |name, i| {
            if (self.typed.get(name)) |e| {
                if (i > 0) try writer.writeAll("\n\n");
                e.toString(writer, Indent{ .value = 0 }) catch unreachable;
            }
        }
    }
};

fn topLevelFunction(allocator: Allocator, builtins: Builtins, f: untyped_ast.Function) !MonoType {
    const len = f.parameters.len;
    const function_type = try allocator.alloc(MonoType, len + 1);
    for (f.parameters, function_type[0..len]) |p, *t|
        t.* = try expressionToMonoType(allocator, builtins, p.type);
    function_type[len] = try expressionToMonoType(allocator, builtins, f.return_type.*);
    return MonoType{ .function = function_type };
}

fn topLevelCall(allocator: Allocator, builtins: Builtins, c: untyped_ast.Call) !MonoType {
    switch (c.function.*) {
        .symbol => |s| {
            if (s.value.eql(builtins.foreign_import)) {
                if (c.arguments.len != 3) std.debug.panic("foreign_import takes 3 arguments", .{});
                return try expressionToMonoType(allocator, builtins, c.arguments[2]);
            }
        },
        else => |k| std.debug.panic("\nInvalid top level call function {}", .{k}),
    }
    std.debug.panic("\nInvalid top level call {}", .{c.function});
}

fn topLevelType(allocator: Allocator, builtins: Builtins, e: untyped_ast.Expression) !MonoType {
    return switch (e) {
        .function => |f| try topLevelFunction(allocator, builtins, f),
        .call => |c| try topLevelCall(allocator, builtins, c),
        else => |k| std.debug.panic("\nInvalid top level value {}", .{k}),
    };
}

pub fn expressionToMonoType(allocator: Allocator, builtins: Builtins, e: untyped_ast.Expression) !MonoType {
    switch (e) {
        .symbol => |s| {
            if (s.value.eql(builtins.i32)) return .i32;
            if (s.value.eql(builtins.i64)) return .i64;
            if (s.value.eql(builtins.f32)) return .f32;
            if (s.value.eql(builtins.f64)) return .f64;
            if (s.value.eql(builtins.bool)) return .bool;
            if (s.value.eql(builtins.str)) return .str;
            if (s.value.eql(builtins.void)) return .void;
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
