const std = @import("std");

const Intern = @import("interner.zig").Intern;
const Indent = @import("indent.zig").Indent;
const token = @import("token.zig");
const CompileErrors = @import("compile_errors.zig").CompileErrors;

const Span = @import("span.zig").Span;
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
            try writer.print(" {}", .{t});
        }
        try writer.writeAll(" ");
        try self.value.toString(writer, indent.add(1));
        try writer.writeAll(")");
    }

    pub fn format(self: Parameter, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try self.toString(writer, Indent{ .value = 0 });
    }
};

pub const Parameter = struct {
    name: Symbol,
    type: Expression,

    pub fn format(self: Parameter, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("({} {})", .{ self.name.value, self.type });
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

    pub fn format(self: Block, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try self.toString(writer, Indent{ .value = 0 });
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
            try writer.print("{}", .{p});
        }
        try writer.print("] {}", .{self.return_type});
        try self.body.toString(writer, indent);
        try writer.writeAll(")");
    }

    pub fn format(self: Function, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try self.toString(writer, Indent{ .value = 0 });
    }
};

pub const Prototype = struct {
    parameters: []const Parameter,
    return_type: *const Expression,
    span: Span,

    pub fn format(self: Prototype, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.writeAll("(fn [");
        for (self.parameters, 0..) |p, j| {
            if (j > 0) try writer.writeAll(" ");
            try writer.print("{}", .{p});
        }
        try writer.print("] {})", .{self.return_type});
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

    pub fn format(self: BinaryOpKind, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
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
        try writer.print("({} ", .{self.kind});
        try self.left.toString(writer, indent);
        try writer.writeAll(" ");
        try self.right.toString(writer, indent);
        try writer.writeAll(")");
    }

    pub fn format(self: BinaryOp, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try self.toString(writer, Indent{ .value = 0 });
    }
};

pub const Group = struct {
    expression: *const Expression,
    span: Span,

    fn toString(self: Group, writer: anytype, indent: Indent) !void {
        try self.expression.toString(writer, indent);
    }

    pub fn format(self: Group, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try self.toString(writer, Indent{ .value = 0 });
    }
};

pub const Arm = struct {
    condition: Expression,
    then: Block,
};

pub const Branch = struct {
    arms: []const Arm,
    else_: Block,
    span: Span,

    fn toString(self: Branch, writer: anytype, indent: Indent) !void {
        try writer.writeAll("(branch");
        for (self.arms) |arm| {
            try writer.print("{}", .{indent});
            try arm.condition.toString(writer, indent);
            try arm.then.toString(writer, indent.add(1));
        }
        try writer.print("{}else", .{indent});
        try self.else_.toString(writer, indent.add(1));
        try writer.writeAll(")");
    }

    pub fn format(self: Branch, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try self.toString(writer, Indent{ .value = 0 });
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

    pub fn format(self: Call, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try self.toString(writer, Indent{ .value = 0 });
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
    branch: Branch,
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
            .branch => |e| e.span,
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
            .prototype => |p| try writer.print("{}", .{p}),
            .binary_op => |b| try b.toString(writer, indent),
            .group => |g| try g.toString(writer, indent),
            .block => |b| try b.toString(writer, indent),
            .branch => |b| try b.toString(writer, indent),
            .call => |c| try c.toString(writer, indent),
        }
    }

    pub fn format(self: Expression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        self.toString(writer, Indent{ .value = 0 }) catch unreachable;
    }
};

pub const Module = struct {
    expressions: []const Expression,
    compile_errors: *CompileErrors,

    pub fn format(self: Module, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        for (self.expressions, 0..) |e, i| {
            if (i > 0) try writer.writeAll("\n\n");
            e.toString(writer, Indent{ .value = 0 }) catch unreachable;
        }
    }
};
