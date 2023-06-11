const std = @import("std");
const List = std.ArrayList;
const Writer = List(u8).Writer;

const types = @import("types.zig");

const Indent = u64;

fn indent(i: Indent, writer: Writer) !void {
    if (i > 0) try writer.writeAll("\n");
    for (0..i) |_| try writer.writeAll("    ");
}

pub fn define(d: types.Define, i: Indent, writer: Writer) !void {
    try writer.writeAll("(def ");
    if (d.mutable) try writer.writeAll("mut ");
    try writer.print("{}", .{d.name.value});
    if (d.type) |t| {
        try writer.writeAll(" ");
        try expression(t.*, i, writer);
    }
    try writer.writeAll(" ");
    try expression(d.value.*, i + 1, writer);
    try writer.writeAll(")");
}

pub fn addAssign(a: types.AddAssign, i: Indent, writer: Writer) !void {
    try writer.print("(+= {} ", .{a.name.value});
    try expression(a.value.*, i + 1, writer);
    try writer.writeAll(")");
}

pub fn function(f: types.Function, i: Indent, writer: Writer) !void {
    try writer.writeAll("(fn [");
    for (f.parameters, 0..) |param, j| {
        if (j > 0) try writer.writeAll(" ");
        try writer.print("({} ", .{param.name.value});
        try expression(param.type, i, writer);
        try writer.writeAll(")");
    }
    try writer.writeAll("] ");
    try expression(f.return_type.*, i, writer);
    try block(f.body, i, writer);
    try writer.writeAll(")");
}

pub fn prototype(p: types.Prototype, i: Indent, writer: Writer) !void {
    try writer.writeAll("(fn [");
    for (p.parameters, 0..) |param, j| {
        if (j > 0) try writer.writeAll(" ");
        try writer.print("({} ", .{param.name.value});
        try expression(param.type, i, writer);
        try writer.writeAll(")");
    }
    try writer.writeAll("] ");
    try expression(p.return_type.*, i, writer);
    try writer.writeAll(")");
}

pub fn binaryOp(b: types.BinaryOp, i: Indent, writer: Writer) !void {
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
    try expression(b.left.*, i, writer);
    try writer.writeAll(" ");
    try expression(b.right.*, i, writer);
    try writer.writeAll(")");
}

pub fn block(b: types.Block, i: Indent, writer: Writer) !void {
    try indent(i, writer);
    if (b.expressions.len == 1) {
        return try expression(b.expressions[0], i + 1, writer);
    }
    try writer.writeAll("(block");
    for (b.expressions) |expr| {
        try indent(i + 1, writer);
        try expression(expr, i + 1, writer);
    }
    try writer.writeAll(")");
}

pub fn array(a: types.Array, i: Indent, writer: Writer) !void {
    try indent(i, writer);
    for (a.expressions) |expr| {
        try indent(i + 1, writer);
        try expression(expr, i + 1, writer);
    }
    try writer.writeAll("]");
}

pub fn arrayOf(a: types.ArrayOf, i: Indent, writer: Writer) !void {
    if (a.size) |size| {
        try writer.print("[{}]", .{size.value});
    } else {
        try writer.writeAll("[]");
    }
    try expression(a.element_type.*, i, writer);
}

pub fn branch(b: types.Branch, i: Indent, writer: Writer) !void {
    try writer.writeAll("(branch");
    for (b.arms) |arm| {
        try indent(i, writer);
        try expression(arm.condition, i, writer);
        try block(arm.then, i + 1, writer);
    }
    try indent(i, writer);
    try writer.writeAll("else");
    try block(b.else_, i + 1, writer);
    try writer.writeAll(")");
}

pub fn call(c: types.Call, i: Indent, writer: Writer) !void {
    try writer.writeAll("(");
    try expression(c.function.*, i, writer);
    for (c.arguments) |a| {
        try writer.writeAll(" ");
        try expression(a, i + 1, writer);
    }
    try writer.writeAll(")");
}

pub fn expression(e: types.Expression, i: Indent, writer: Writer) error{OutOfMemory}!void {
    switch (e) {
        .int => |int| try writer.print("{}", .{int.value}),
        .float => |f| try writer.print("{}", .{f.value}),
        .symbol => |s| try writer.print("{}", .{s.value}),
        .string => |s| try writer.print("{}", .{s.value}),
        .bool => |b| try writer.print("{}", .{b.value}),
        .define => |d| try define(d, i, writer),
        .add_assign => |a| try addAssign(a, i, writer),
        .function => |f| try function(f, i, writer),
        .prototype => |p| try prototype(p, i, writer),
        .binary_op => |b| try binaryOp(b, i, writer),
        .group => |g| try expression(g.expression.*, i, writer),
        .block => |b| try block(b, i, writer),
        .array => |a| try array(a, i, writer),
        .array_of => |a| try arrayOf(a, i, writer),
        .branch => |b| try branch(b, i, writer),
        .call => |c| try call(c, i, writer),
        .undefined => |u| try writer.print("{}", .{u}),
    }
}

pub fn module(m: types.Module, writer: Writer) !void {
    for (m.expressions, 0..) |e, i| {
        if (i > 0) try writer.writeAll("\n\n");
        try expression(e, 0, writer);
    }
}
