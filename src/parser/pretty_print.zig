const std = @import("std");
const List = std.ArrayList;
const Writer = List(u8).Writer;

const types = @import("types.zig");

const Indent = u64;

fn newlineAndIndent(indent: Indent, writer: Writer) !void {
    if (indent > 0) try writer.writeAll("\n");
    for (0..indent) |_| try writer.writeAll("    ");
}

pub fn define(d: types.Define, indent: Indent, writer: Writer) !void {
    try writer.writeAll("(def ");
    if (d.mutable) try writer.writeAll("mut ");
    try writer.print("{}", .{d.name.value});
    if (d.type) |t| {
        try writer.writeAll(" ");
        try expression(t.*, indent, writer);
    }
    try writer.writeAll(" ");
    try expression(d.value.*, indent + 1, writer);
    try writer.writeAll(")");
}

pub fn addAssign(a: types.AddAssign, indent: Indent, writer: Writer) !void {
    try writer.print("(+= {} ", .{a.name.value});
    try expression(a.value.*, indent + 1, writer);
    try writer.writeAll(")");
}

pub fn function(f: types.Function, indent: Indent, writer: Writer) !void {
    try writer.writeAll("(fn [");
    for (f.parameters, 0..) |param, j| {
        if (j > 0) try writer.writeAll(" ");
        try writer.print("({} ", .{param.name.value});
        try expression(param.type, indent, writer);
        try writer.writeAll(")");
    }
    try writer.writeAll("] ");
    try expression(f.return_type.*, indent, writer);
    try block(f.body, indent, writer);
    try writer.writeAll(")");
}

pub fn prototype(p: types.Prototype, indent: Indent, writer: Writer) !void {
    try writer.writeAll("(fn [");
    for (p.parameters, 0..) |param, j| {
        if (j > 0) try writer.writeAll(" ");
        try writer.print("({} ", .{param.name.value});
        try expression(param.type, indent, writer);
        try writer.writeAll(")");
    }
    try writer.writeAll("] ");
    try expression(p.return_type.*, indent, writer);
    try writer.writeAll(")");
}

pub fn binaryOp(b: types.BinaryOp, indent: Indent, writer: Writer) !void {
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
        .dot => try writer.writeAll("."),
    }
    try writer.writeAll(" ");
    try expression(b.left.*, indent, writer);
    try writer.writeAll(" ");
    try expression(b.right.*, indent, writer);
    try writer.writeAll(")");
}

pub fn block(b: types.Block, indent: Indent, writer: Writer) !void {
    try newlineAndIndent(indent, writer);
    if (b.expressions.len == 1) {
        return try expression(b.expressions[0], indent + 1, writer);
    }
    try writer.writeAll("(block");
    for (b.expressions) |expr| {
        try newlineAndIndent(indent + 1, writer);
        try expression(expr, indent + 1, writer);
    }
    try writer.writeAll(")");
}

pub fn array(a: types.Array, indent: Indent, writer: Writer) !void {
    try newlineAndIndent(indent, writer);
    for (a.expressions) |expr| {
        try newlineAndIndent(indent + 1, writer);
        try expression(expr, indent + 1, writer);
    }
    try writer.writeAll("]");
}

pub fn arrayOf(a: types.ArrayOf, indent: Indent, writer: Writer) !void {
    if (a.size) |size| {
        try writer.print("[{}]", .{size.value});
    } else {
        try writer.writeAll("[]");
    }
    try expression(a.element_type.*, indent, writer);
}

pub fn branch(b: types.Branch, indent: Indent, writer: Writer) !void {
    try writer.writeAll("(branch");
    for (b.arms) |arm| {
        try newlineAndIndent(indent, writer);
        try expression(arm.condition, indent, writer);
        try block(arm.then, indent + 1, writer);
    }
    try newlineAndIndent(indent, writer);
    try writer.writeAll("else");
    try block(b.else_, indent + 1, writer);
    try writer.writeAll(")");
}

pub fn call(c: types.Call, indent: Indent, writer: Writer) !void {
    try writer.writeAll("(");
    try expression(c.function.*, indent, writer);
    for (c.arguments) |a| {
        try writer.writeAll(" ");
        try expression(a, indent + 1, writer);
    }
    try writer.writeAll(")");
}

pub fn expression(e: types.Expression, indent: Indent, writer: Writer) error{OutOfMemory}!void {
    switch (e) {
        .int => |int| try writer.print("{}", .{int.value}),
        .float => |f| try writer.print("{}", .{f.value}),
        .symbol => |s| try writer.print("{}", .{s.value}),
        .string => |s| try writer.print("{}", .{s.value}),
        .bool => |b| try writer.print("{}", .{b.value}),
        .define => |d| try define(d, indent, writer),
        .add_assign => |a| try addAssign(a, indent, writer),
        .function => |f| try function(f, indent, writer),
        .prototype => |p| try prototype(p, indent, writer),
        .binary_op => |b| try binaryOp(b, indent, writer),
        .group => |g| try expression(g.expression.*, indent, writer),
        .block => |b| try block(b, indent, writer),
        .array => |a| try array(a, indent, writer),
        .array_of => |a| try arrayOf(a, indent, writer),
        .branch => |b| try branch(b, indent, writer),
        .call => |c| try call(c, indent, writer),
        .undefined => |u| try writer.print("{}", .{u}),
    }
}

pub fn module(m: types.Module, writer: Writer) !void {
    for (m.expressions, 0..) |e, i| {
        if (i > 0) try writer.writeAll("\n\n");
        try expression(e, 0, writer);
    }
}
