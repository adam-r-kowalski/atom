const std = @import("std");
const List = std.ArrayList;
const Allocator = std.mem.Allocator;

const interner = @import("../interner.zig");
const Interned = interner.Interned;
const Intern = interner.Intern;
const types = @import("types.zig");
const Expression = types.Expression;
const Define = types.Define;
const Function = types.Function;
const Prototype = types.Prototype;
const BinaryOp = types.BinaryOp;
const If = types.If;
const Parameter = types.Parameter;
const Block = types.Block;
const Call = types.Call;
const Module = types.Module;

fn interned(writer: List(u8).Writer, intern: Intern, s: Interned) !void {
    try writer.writeAll(interner.lookup(intern, s));
}

fn typeString(writer: List(u8).Writer, intern: Intern, expr: Expression) !void {
    switch (expr) {
        .symbol => |symbol| try interned(writer, intern, symbol.value),
        else => |k| std.debug.panic("\nType string not implemented for {}", .{k}),
    }
}

const Indent = u64;

fn indent(writer: List(u8).Writer, n: Indent) !void {
    try writer.writeAll("\n");
    for (0..n) |_| try writer.writeAll("    ");
}

fn define(writer: List(u8).Writer, intern: Intern, d: Define, i: Indent) !void {
    try writer.writeAll("(def ");
    try interned(writer, intern, d.name.value);
    if (d.type) |t| {
        try writer.writeAll(" ");
        try typeString(writer, intern, t.*);
    }
    try writer.writeAll(" ");
    try expression(writer, intern, d.value.*, i + 1);
    try writer.writeAll(")");
}

fn parameter(writer: List(u8).Writer, intern: Intern, p: Parameter) !void {
    try writer.writeAll("(");
    try interned(writer, intern, p.name.value);
    try writer.writeAll(" ");
    try typeString(writer, intern, p.type);
    try writer.writeAll(")");
}

fn function(writer: List(u8).Writer, intern: Intern, f: Function, i: Indent) !void {
    try writer.writeAll("(fn [");
    for (f.parameters, 0..) |p, j| {
        if (j > 0) try writer.writeAll(" ");
        try parameter(writer, intern, p);
    }
    try writer.writeAll("] ");
    try typeString(writer, intern, f.return_type.*);
    try block(writer, intern, f.body, i);
    try writer.writeAll(")");
}

fn prototype(writer: List(u8).Writer, intern: Intern, p: Prototype) !void {
    try writer.writeAll("(fn [");
    for (p.parameters, 0..) |param, j| {
        if (j > 0) try writer.writeAll(" ");
        try parameter(writer, intern, param);
    }
    try writer.writeAll("] ");
    try typeString(writer, intern, p.return_type.*);
    try writer.writeAll(")");
}

fn binaryOp(writer: List(u8).Writer, intern: Intern, b: BinaryOp, i: Indent) !void {
    try writer.writeAll("(");
    switch (b.kind) {
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
    }
    try writer.writeAll(" ");
    try expression(writer, intern, b.left.*, i);
    try writer.writeAll(" ");
    try expression(writer, intern, b.right.*, i);
    try writer.writeAll(")");
}

fn conditional(writer: List(u8).Writer, intern: Intern, i: If, n: Indent) !void {
    try writer.writeAll("(if ");
    try expression(writer, intern, i.condition.*, n);
    try block(writer, intern, i.then, n);
    try block(writer, intern, i.else_, n);
    try writer.writeAll(")");
}

fn call(writer: List(u8).Writer, intern: Intern, c: Call, i: u64) !void {
    try writer.writeAll("(");
    try expression(writer, intern, c.function.*, i);
    for (c.arguments) |a| {
        try writer.writeAll(" ");
        try expression(writer, intern, a, i + 1);
    }
    try writer.writeAll(")");
}

fn block(writer: List(u8).Writer, intern: Intern, b: Block, i: Indent) !void {
    try indent(writer, i);
    if (b.expressions.len == 1) {
        return try expression(writer, intern, b.expressions[0], i + 1);
    }
    try writer.writeAll("(block");
    for (b.expressions) |expr| {
        try indent(writer, i + 1);
        try expression(writer, intern, expr, i + 1);
    }
    try writer.writeAll(")");
}

fn expression(writer: List(u8).Writer, intern: Intern, expr: Expression, n: Indent) error{OutOfMemory}!void {
    switch (expr) {
        .int => |i| try interned(writer, intern, i.value),
        .float => |f| try interned(writer, intern, f.value),
        .symbol => |s| try interned(writer, intern, s.value),
        .string => |s| try interned(writer, intern, s.value),
        .bool => |b| try writer.print("{}", .{b.value}),
        .define => |d| try define(writer, intern, d, n),
        .function => |f| try function(writer, intern, f, n),
        .prototype => |p| try prototype(writer, intern, p),
        .binary_op => |b| try binaryOp(writer, intern, b, n),
        .group => |g| try expression(writer, intern, g.expression.*, n),
        .block => |b| try block(writer, intern, b, n),
        .if_ => |i| try conditional(writer, intern, i, n),
        .call => |c| try call(writer, intern, c, n),
    }
}

pub fn toString(allocator: Allocator, intern: Intern, module: Module) ![]u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    for (module.expressions, 0..) |e, i| {
        if (i > 0) try writer.writeAll("\n\n");
        try expression(writer, intern, e, 0);
    }
    return list.toOwnedSlice();
}
