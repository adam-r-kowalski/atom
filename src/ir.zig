const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const typed_ast = @import("typed_ast.zig");
const Module = typed_ast.Module;
const MonoType = @import("substitution.zig").MonoType;
const Builtins = @import("builtins.zig").Builtins;
const Indent = @import("indent.zig").Indent;

pub const Type = union(enum) {
    i32,
    i64,
    f32,
    f64,
    void,
    function: []const Type,

    pub fn format(self: Type, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .i32 => try writer.writeAll("i32"),
            .i64 => try writer.writeAll("i64"),
            .f32 => try writer.writeAll("f32"),
            .f64 => try writer.writeAll("f64"),
            .void => try writer.writeAll("void"),
            .function => |f| {
                const last = f.len - 1;
                for (f[0..last], 0..) |arg, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("(param {})", .{arg});
                }
                switch (f[last]) {
                    .void => {},
                    else => |k| try writer.print(" (result {})", .{k}),
                }
            },
        }
    }
};

pub const Parameter = struct {
    name: Interned,
    type: Type,

    pub fn format(self: Parameter, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(param ${} {})", .{ self.name, self.type });
    }
};

pub const BinaryOpKind = enum {
    i32_add,
    i32_sub,
    i32_mul,
    i32_div_s,
    i32_eq,
    i32_gt_s,
    i32_lt_s,
    i32_rem_s,
    i32_or,
    i64_add,
    i64_sub,
    i64_mul,
    i64_div_s,
    i64_eq,
    i64_gt_s,
    i64_lt_s,
    i64_rem_s,
    f32_add,
    f32_sub,
    f32_mul,
    f32_div,
    f32_eq,
    f32_gt,
    f32_lt,
    f64_add,
    f64_sub,
    f64_mul,
    f64_div,
    f64_eq,
    f64_gt,
    f64_lt,

    pub fn format(self: BinaryOpKind, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .i32_add => try writer.writeAll("i32.add"),
            .i32_sub => try writer.writeAll("i32.sub"),
            .i32_mul => try writer.writeAll("i32.mul"),
            .i32_div_s => try writer.writeAll("i32.div_s"),
            .i32_eq => try writer.writeAll("i32.eq"),
            .i32_rem_s => try writer.writeAll("i32.rem_s"),
            .i32_or => try writer.writeAll("i32.or"),
            .i32_gt_s => try writer.writeAll("i32.gt_s"),
            .i32_lt_s => try writer.writeAll("i32.lt_s"),
            .i64_add => try writer.writeAll("i64.add"),
            .i64_sub => try writer.writeAll("i64.sub"),
            .i64_mul => try writer.writeAll("i64.mul"),
            .i64_div_s => try writer.writeAll("i64.div_s"),
            .i64_eq => try writer.writeAll("i64.eq"),
            .i64_rem_s => try writer.writeAll("i64.rem_s"),
            .i64_gt_s => try writer.writeAll("i64.gt_s"),
            .i64_lt_s => try writer.writeAll("i64.lt_s"),
            .f32_add => try writer.writeAll("f32.add"),
            .f32_sub => try writer.writeAll("f32.sub"),
            .f32_mul => try writer.writeAll("f32.mul"),
            .f32_div => try writer.writeAll("f32.div"),
            .f32_eq => try writer.writeAll("f32.eq"),
            .f32_gt => try writer.writeAll("f32.gt"),
            .f32_lt => try writer.writeAll("f32.lt"),
            .f64_add => try writer.writeAll("f64.add"),
            .f64_sub => try writer.writeAll("f64.sub"),
            .f64_mul => try writer.writeAll("f64.mul"),
            .f64_div => try writer.writeAll("f64.div"),
            .f64_eq => try writer.writeAll("f64.eq"),
            .f64_gt => try writer.writeAll("f64.gt"),
            .f64_lt => try writer.writeAll("f64.lt"),
        }
    }
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *const Expression,
    right: *const Expression,

    fn toString(self: BinaryOp, writer: anytype, i: Indent) !void {
        try writer.print("({}{}", .{ self.kind, i.add(1) });
        self.left.toString(writer, i.add(1)) catch unreachable;
        try writer.print("{}", .{i.add(1)});
        self.right.toString(writer, i.add(1)) catch unreachable;
        try writer.writeAll(")");
    }

    pub fn format(self: BinaryOp, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try self.toString(writer, Indent{ .value = 0 });
    }
};

const UnaryOpKind = enum {
    i32_trunc_f32_s,
    i64_trunc_f64_s,
    f32_sqrt,
    f32_convert_i32_s,
    f64_sqrt,
    f64_convert_i64_s,

    pub fn format(self: UnaryOpKind, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .i32_trunc_f32_s => try writer.writeAll("i32.trunc_f32_s"),
            .i64_trunc_f64_s => try writer.writeAll("i64.trunc_f64_s"),
            .f32_sqrt => try writer.writeAll("f32.sqrt"),
            .f32_convert_i32_s => try writer.writeAll("f32.convert_i32_s"),
            .f64_sqrt => try writer.writeAll("f64.sqrt"),
            .f64_convert_i64_s => try writer.writeAll("f64.convert_i64_s"),
        }
    }
};

pub const UnaryOp = struct {
    kind: UnaryOpKind,
    expression: *const Expression,

    fn toString(self: UnaryOp, writer: anytype, i: Indent) !void {
        try writer.print("({}{}", .{ self.kind, i });
        self.expression.toString(writer, i) catch unreachable;
        try writer.writeAll(")");
    }

    pub fn format(self: UnaryOp, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try self.toString(writer, Indent{ .value = 0 });
    }
};

pub const Call = struct {
    function: Interned,
    arguments: []const Expression,

    fn toString(self: Call, writer: anytype, indent: Indent) !void {
        try writer.print("(call ${}", .{self.function});
        for (self.arguments) |arg| {
            try writer.print("{}", .{indent.add(1)});
            arg.toString(writer, indent.add(1)) catch unreachable;
        }
        try writer.writeAll(")");
    }
};

pub const If = struct {
    result: Type,
    condition: *const Expression,
    then: Block,
    else_: Block,

    fn toString(self: If, writer: anytype, indent: Indent) !void {
        try writer.writeAll("(if ");
        switch (self.result) {
            .void => {},
            else => |t| try writer.print("(result {})", .{t}),
        }
        try writer.print("{}", .{indent.add(1)});
        self.condition.toString(writer, indent.add(1)) catch unreachable;
        try writer.print("{}(then", .{indent.add(1)});
        try self.then.toString(writer, indent.add(2));
        try writer.writeAll(")");
        if (self.else_.expressions.len > 0) {
            try writer.print("{}(else", .{indent.add(1)});
            try self.else_.toString(writer, indent.add(2));
            try writer.writeAll(")");
        }
        try writer.writeAll(")");
    }
};

pub const I32Const = struct {
    value: Interned,

    pub fn format(self: I32Const, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(i32.const {})", .{self.value});
    }
};

pub const I64Const = struct {
    value: Interned,

    pub fn format(self: I64Const, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(i64.const {})", .{self.value});
    }
};

pub const F32Const = struct {
    value: Interned,

    pub fn format(self: F32Const, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(f32.const {})", .{self.value});
    }
};

pub const F64Const = struct {
    value: Interned,

    pub fn format(self: F64Const, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(f64.const {})", .{self.value});
    }
};

pub const LocalGet = struct {
    name: Interned,

    pub fn format(self: LocalGet, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(local.get ${})", .{self.name});
    }
};

pub const LocalSet = struct {
    name: Interned,
    value: *const Expression,

    pub fn toString(self: LocalSet, writer: anytype, indent: Indent) !void {
        try writer.print("(local.set ${}{}", .{ self.name, indent.add(1) });
        self.value.toString(writer, indent.add(1)) catch unreachable;
        try writer.writeAll(")");
    }

    pub fn format(self: LocalSet, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try self.toString(writer, Indent{ .value = 0 });
    }
};

pub const Block = struct {
    expressions: []const Expression,

    pub fn toString(self: Block, writer: anytype, indent: Indent) !void {
        for (self.expressions) |expr| {
            try writer.print("{}", .{indent});
            expr.toString(writer, indent) catch unreachable;
        }
    }
};

pub const Expression = union(enum) {
    local_get: LocalGet,
    local_set: LocalSet,
    i32_const: I32Const,
    i64_const: I64Const,
    f32_const: F32Const,
    f64_const: F64Const,
    call: Call,
    if_: If,
    unary_op: UnaryOp,
    binary_op: BinaryOp,
    block: Block,

    pub fn toString(self: Expression, writer: anytype, indent: Indent) error{NoSpaceLeft}!void {
        switch (self) {
            .local_get => |l| try writer.print("{}", .{l}),
            .local_set => |l| try l.toString(writer, indent),
            .i32_const => |i| try writer.print("{}", .{i}),
            .i64_const => |i| try writer.print("{}", .{i}),
            .f32_const => |f| try writer.print("{}", .{f}),
            .f64_const => |f| try writer.print("{}", .{f}),
            .call => |c| try c.toString(writer, indent),
            .if_ => |i| try i.toString(writer, indent),
            .unary_op => |u| try u.toString(writer, indent),
            .binary_op => |b| try b.toString(writer, indent),
            .block => |b| try b.toString(writer, indent),
        }
    }

    pub fn format(self: Expression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try self.toString(writer, Indent{ .value = 1 });
    }
};

pub const Local = struct {
    name: Interned,
    type: Type,

    pub fn format(self: Local, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(local ${} {})", .{ self.name, self.type });
    }
};

pub const Function = struct {
    name: Interned,
    parameters: []const Parameter,
    return_type: Type,
    locals: []const Local,
    body: Block,

    pub fn format(self: Function, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(func ${}", .{self.name});
        for (self.parameters) |p| try writer.print(" {p}", .{p});
        switch (self.return_type) {
            .void => {},
            else => try writer.print(" (result {})", .{self.return_type}),
        }
        for (self.locals) |l| try writer.print("{}{}", .{ Indent{ .value = 2 }, l });
        try self.body.toString(writer, Indent{ .value = 2 });
        try writer.writeAll(")");
    }
};

pub const Import = struct {
    name: Interned,
    path: [2]Interned,
    type: Type,

    pub fn format(self: Import, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(import {} {} (func ${} {}))", .{ self.path[0], self.path[1], self.name, self.type });
    }
};

pub const Export = struct {
    name: Interned,
    alias: Interned,

    pub fn format(self: Export, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(export \"{}\" (func ${}))", .{ self.alias, self.name });
    }
};

pub const IR = struct {
    functions: []const Function,
    imports: []const Import,
    exports: []const Export,

    pub fn format(self: IR, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.writeAll("(module");
        for (self.imports) |i| try writer.print("\n{}{}", .{ Indent{ .value = 1 }, i });
        for (self.functions) |f| try writer.print("\n{}{}", .{ Indent{ .value = 1 }, f });
        for (self.exports) |e| try writer.print("\n{}{}", .{ Indent{ .value = 1 }, e });
        try writer.writeAll(")");
    }
};
