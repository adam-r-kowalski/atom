const std = @import("std");

const Intern = @import("interner.zig").Intern;
const Indent = @import("indent.zig").Indent;
const token = @import("token.zig");

pub const Span = token.Span;
pub const Int = token.Int;
pub const Float = token.Float;
pub const Symbol = token.Symbol;
pub const String = token.String;
pub const Bool = token.Bool;

pub const Define = struct {
    name: Symbol,
    type: ?*const Expression,
    value: *const Expression,
    span: Span,

    fn toString(self: Define, writer: anytype, indent: Indent) !void {
        try writer.print("(def {}", .{self.name.value});
        if (self.type) |t| {
            try writer.writeAll(" ");
            try t.toString(writer, Indent{ .value = 0 });
        }
        try writer.writeAll(" ");
        try self.value.toString(writer, indent.add(1));
        try writer.writeAll(")");
    }
};

pub const Parameter = struct {
    name: Symbol,
    type: Expression,

    fn toString(self: Parameter, writer: anytype) !void {
        try writer.print("({} ", .{self.name.value});
        try self.type.toString(writer, Indent{ .value = 0 });
        try writer.writeAll(")");
    }
};

pub const Block = struct {
    expressions: []const Expression,
    span: Span,

    fn toString(self: Block, writer: anytype, indent: Indent) !void {
        try writer.print("{}", .{indent});
        if (self.expressions.len == 1) {
            return try self.expressions[0].toString(writer, indent.add(1));
        }
        try writer.writeAll("(block");
        for (self.expressions) |expr| {
            try writer.print("{}", .{indent.add(1)});
            try expr.toString(writer, indent.add(1));
        }
        try writer.writeAll(")");
    }
};

pub const Function = struct {
    parameters: []const Parameter,
    return_type: *const Expression,
    body: Block,
    span: Span,

    fn toString(self: Function, writer: anytype, indent: Indent) !void {
        try writer.writeAll("(fn [");
        for (self.parameters, 0..) |p, j| {
            if (j > 0) try writer.writeAll(" ");
            try p.toString(writer);
        }
        try writer.writeAll("] ");
        try self.return_type.toString(writer, indent);
        try self.body.toString(writer, indent);
        try writer.writeAll(")");
    }
};

pub const Prototype = struct {
    parameters: []const Parameter,
    return_type: *const Expression,
    span: Span,

    fn toString(self: Prototype, writer: anytype) !void {
        try writer.writeAll("(fn [");
        for (self.parameters, 0..) |p, j| {
            if (j > 0) try writer.writeAll(" ");
            try p.toString(writer);
        }
        try writer.writeAll("] ");
        try self.return_type.toString(writer, Indent{ .value = 0 });
        try writer.writeAll(")");
    }
};

pub const BinaryOpKind = enum {
    add,
    subtract,
    multiply,
    divide,
    modulo,
    exponentiate,
    equal,
    greater,
    less,
    or_,
    dot,

    pub fn toString(self: BinaryOpKind, writer: anytype) !void {
        switch (self) {
            .add => try writer.writeAll("+"),
            .subtract => try writer.writeAll("-"),
            .multiply => try writer.writeAll("*"),
            .divide => try writer.writeAll("/"),
            .modulo => try writer.writeAll("%"),
            .exponentiate => try writer.writeAll("^"),
            .equal => try writer.writeAll("=="),
            .greater => try writer.writeAll(">"),
            .less => try writer.writeAll("<"),
            .or_ => try writer.writeAll("or"),
            .dot => try writer.writeAll("."),
        }
    }
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *const Expression,
    right: *const Expression,
    span: Span,

    fn toString(self: BinaryOp, writer: anytype, indent: Indent) !void {
        try writer.writeAll("(");
        try self.kind.toString(writer);
        try writer.writeAll(" ");
        try self.left.toString(writer, indent);
        try writer.writeAll(" ");
        try self.right.toString(writer, indent);
        try writer.writeAll(")");
    }
};

pub const Group = struct {
    expression: *const Expression,
    span: Span,

    fn toString(self: Group, writer: anytype, indent: Indent) !void {
        try self.expression.toString(writer, indent);
    }
};

pub const If = struct {
    condition: *const Expression,
    then: Block,
    else_: Block,
    span: Span,

    fn toString(self: If, writer: anytype, indent: Indent) !void {
        try writer.writeAll("(if ");
        try self.condition.toString(writer, indent);
        try self.then.toString(writer, indent);
        try self.else_.toString(writer, indent);
        try writer.writeAll(")");
    }
};

pub const Cond = struct {
    conditions: []const Expression,
    thens: []const Block,
    else_: Block,
    span: Span,

    fn toString(self: Cond, writer: anytype, indent: Indent) !void {
        try writer.writeAll("(cond");
        for (self.conditions, self.thens) |b, t| {
            try writer.print("{}", .{indent});
            try b.toString(writer, indent);
            try t.toString(writer, indent.add(1));
        }
        try writer.print("{}", .{indent});
        try writer.writeAll("else");
        try self.else_.toString(writer, indent.add(1));
        try writer.writeAll(")");
    }
};

pub const Call = struct {
    function: *const Expression,
    arguments: []const Expression,
    span: Span,

    fn toString(self: Call, writer: anytype, indent: Indent) !void {
        try writer.writeAll("(");
        try self.function.toString(writer, indent);
        for (self.arguments) |a| {
            try writer.writeAll(" ");
            try a.toString(writer, indent.add(1));
        }
        try writer.writeAll(")");
    }
};

pub const Expression = union(enum) {
    int: Int,
    float: Float,
    symbol: Symbol,
    string: String,
    bool: Bool,
    define: Define,
    function: Function,
    prototype: Prototype,
    binary_op: BinaryOp,
    group: Group,
    block: Block,
    if_else: If,
    cond: Cond,
    call: Call,

    pub fn span(self: Expression) Span {
        return switch (self) {
            .int => |e| e.span,
            .float => |e| e.span,
            .symbol => |e| e.span,
            .string => |e| e.span,
            .bool => |e| e.span,
            .define => |e| e.span,
            .function => |e| e.span,
            .prototype => |e| e.span,
            .binary_op => |e| e.span,
            .group => |e| e.span,
            .block => |e| e.span,
            .if_else => |e| e.span,
            .cond => |e| e.span,
            .call => |e| e.span,
        };
    }

    fn toString(self: Expression, writer: anytype, indent: Indent) error{NoSpaceLeft}!void {
        switch (self) {
            .int => |i| try writer.print("{}", .{i.value}),
            .float => |f| try writer.print("{}", .{f.value}),
            .symbol => |s| try writer.print("{}", .{s.value}),
            .string => |s| try writer.print("{}", .{s.value}),
            .bool => |b| try writer.print("{}", .{b.value}),
            .define => |d| try d.toString(writer, indent),
            .function => |f| try f.toString(writer, indent),
            .prototype => |p| try p.toString(writer),
            .binary_op => |b| try b.toString(writer, indent),
            .group => |g| try g.toString(writer, indent),
            .block => |b| try b.toString(writer, indent),
            .if_else => |i| try i.toString(writer, indent),
            .cond => |c| try c.toString(writer, indent),
            .call => |c| try c.toString(writer, indent),
        }
    }
};

pub const Module = struct {
    expressions: []const Expression,

    pub fn format(self: Module, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        for (self.expressions, 0..) |e, i| {
            if (i > 0) try writer.writeAll("\n\n");
            e.toString(writer, Indent{ .value = 0 }) catch unreachable;
        }
    }
};
