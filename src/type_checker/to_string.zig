const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const types = @import("types.zig");
const Module = types.Module;
const TopLevel = types.TopLevel;
const Function = types.Function;
const Symbol = types.Symbol;
const Int = types.Int;
const Float = types.Float;
const Bool = types.Bool;
const MonoType = types.MonoType;
const Expression = types.Expression;
const Constraints = types.Constraints;
const Equal = types.Equal;
const Substitution = types.Substitution;
const If = types.If;
const BinaryOp = types.BinaryOp;
const Call = types.Call;
const Define = types.Define;

const Indent = u64;

pub fn indent(writer: List(u8).Writer, n: Indent) !void {
    if (n > 0) try writer.writeAll("\n");
    var i: u64 = 0;
    while (i < n) : (i += 1) try writer.writeAll("    ");
}

fn symbol(writer: List(u8).Writer, intern: Intern, s: Symbol) !void {
    const name = interner.lookup(intern, s.value);
    try writer.print("symbol{{ name = {s}, type = ", .{name});
    try monotype(writer, s.type);
    try writer.writeAll(" }");
}

fn int(writer: List(u8).Writer, intern: Intern, i: Int) !void {
    const value = interner.lookup(intern, i.value);
    try writer.print("int{{ value = {s}, type = ", .{value});
    try monotype(writer, i.type);
    try writer.writeAll(" }");
}

fn float(writer: List(u8).Writer, intern: Intern, f: Float) !void {
    const value = interner.lookup(intern, f.value);
    try writer.print("float{{ value = {s}, type = ", .{value});
    try monotype(writer, f.type);
    try writer.writeAll(" }");
}

fn boolean(writer: List(u8).Writer, b: Bool) !void {
    try writer.print("bool{{ value = {}, type = ", .{b.value});
    try monotype(writer, b.type);
    try writer.writeAll(" }");
}

pub fn monotype(writer: List(u8).Writer, m: MonoType) !void {
    switch (m) {
        .i32 => try writer.writeAll("i32"),
        .f32 => try writer.writeAll("f32"),
        .bool => try writer.writeAll("bool"),
        .void => try writer.writeAll("void"),
        .typevar => |t| try writer.print("${}", .{t}),
        .function => |f| {
            try writer.writeAll("(");
            for (f) |a, i| {
                if (i == f.len - 1) {
                    try writer.writeAll(") -> ");
                } else if (i > 0) {
                    try writer.writeAll(", ");
                }
                try monotype(writer, a);
            }
        },
        else => std.debug.panic("\nUnhandled monotype type {}", .{m}),
    }
}

fn if_(writer: List(u8).Writer, intern: Intern, i: If, in: Indent) !void {
    try indent(writer, in);
    try writer.writeAll("if =");
    try indent(writer, in + 1);
    try writer.writeAll("condition = ");
    try expression(writer, intern, i.condition.*, in + 2);
    try indent(writer, in + 1);
    try writer.writeAll("then = ");
    try block(writer, intern, i.then, in + 2);
    try indent(writer, in + 1);
    try writer.writeAll("else = ");
    try block(writer, intern, i.else_, in + 2);
    try indent(writer, in + 1);
    try writer.print("type = ", .{});
    try monotype(writer, i.type);
}

fn binaryOp(writer: List(u8).Writer, intern: Intern, b: BinaryOp, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("binary_op =");
    try indent(writer, i + 1);
    try writer.writeAll("kind = ");
    switch (b.kind) {
        .add => try writer.writeAll("+"),
        .multiply => try writer.writeAll("*"),
        else => unreachable,
    }
    try indent(writer, i + 1);
    try writer.writeAll("left = ");
    try expression(writer, intern, b.left.*, i + 2);
    try indent(writer, i + 1);
    try writer.writeAll("right = ");
    try expression(writer, intern, b.right.*, i + 2);
    try indent(writer, i + 1);
    try writer.print("type = ", .{});
    try monotype(writer, b.type);
}

fn call(writer: List(u8).Writer, intern: Intern, c: Call, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("call =");
    try indent(writer, i + 1);
    try expression(writer, intern, c.function.*, i + 2);
    try indent(writer, i + 1);
    try writer.writeAll("arguments =");
    for (c.arguments) |a| {
        try indent(writer, i + 2);
        try expression(writer, intern, a, i + 3);
    }
    try indent(writer, i + 1);
    try writer.print("type = ", .{});
    try monotype(writer, c.type);
}

fn define(writer: List(u8).Writer, intern: Intern, d: Define, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("define =");
    try indent(writer, i + 1);
    try writer.writeAll("name = ");
    try symbol(writer, intern, d.name);
    try indent(writer, i + 1);
    try writer.writeAll("type = ");
    try monotype(writer, d.type);
    try indent(writer, i + 1);
    try writer.writeAll("body = ");
    try block(writer, intern, d.body, i + 2);
}

fn expression(writer: List(u8).Writer, intern: Intern, expr: Expression, in: Indent) error{OutOfMemory}!void {
    switch (expr) {
        .symbol => |s| try symbol(writer, intern, s),
        .int => |i| try int(writer, intern, i),
        .float => |f| try float(writer, intern, f),
        .bool => |b| try boolean(writer, b),
        .if_ => |i| try if_(writer, intern, i, in),
        .binary_op => |b| try binaryOp(writer, intern, b, in),
        .call => |c| try call(writer, intern, c, in),
        .define => |d| try define(writer, intern, d, in),
        else => std.debug.panic("\nUnhandled expression type {}", .{expr}),
    }
}

fn block(writer: List(u8).Writer, intern: Intern, exprs: []const Expression, i: Indent) !void {
    for (exprs) |expr| {
        try expression(writer, intern, expr, i);
    }
}

fn function(writer: List(u8).Writer, intern: Intern, f: Function, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("function");
    try indent(writer, i + 1);
    const name = interner.lookup(intern, f.name.value);
    try writer.print("name = {s}", .{name});
    if (f.parameters.len != 0) {
        try indent(writer, i + 1);
        try writer.print("parameters =", .{});
    }
    for (f.parameters) |p| {
        try indent(writer, i + 2);
        try symbol(writer, intern, p);
    }
    try indent(writer, i + 1);
    try writer.print("return_type = ", .{});
    try monotype(writer, f.return_type);
    try indent(writer, i + 1);
    try writer.print("body = ", .{});
    try block(writer, intern, f.body, i + 2);
}

fn topLevel(writer: List(u8).Writer, intern: Intern, t: TopLevel) !void {
    switch (t) {
        .function => |f| try function(writer, intern, f, 0),
        else => std.debug.panic("\nUnhandled top level type {}", .{t}),
    }
}

pub fn toString(writer: List(u8).Writer, intern: Intern, m: Module) !void {
    for (m.order) |name, i| {
        if (m.typed.get(name)) |t| {
            if (i > 0) try writer.writeAll("\n\n");
            try topLevel(writer, intern, t);
        }
    }
}
