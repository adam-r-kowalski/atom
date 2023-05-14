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
const Bool = types.Bool;
const MonoType = types.MonoType;
const Expression = types.Expression;
const Constraints = types.Constraints;
const Equal = types.Equal;
const Substitution = types.Substitution;
const If = types.If;
const BinaryOp = types.BinaryOp;

const Indent = u64;

fn indent(writer: List(u8).Writer, n: Indent) !void {
    if (n > 0) try writer.writeAll("\n");
    var i: u64 = 0;
    while (i < n) : (i += 1) try writer.writeAll("    ");
}

fn symbol(writer: List(u8).Writer, intern: Intern, s: Symbol, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("symbol =");
    try indent(writer, i + 1);
    const name = interner.lookup(intern, s.value);
    try writer.print("name = {s}", .{name});
    try indent(writer, i + 1);
    try writer.print("type = ", .{});
    try monotype(writer, intern, s.type);
}

fn int(writer: List(u8).Writer, intern: Intern, i: Int, in: Indent) !void {
    try indent(writer, in);
    try writer.writeAll("int =");
    try indent(writer, in + 1);
    const value = interner.lookup(intern, i.value);
    try writer.print("value = {s}", .{value});
    try indent(writer, in + 1);
    try writer.print("type = ", .{});
    try monotype(writer, intern, i.type);
}

fn boolean(writer: List(u8).Writer, intern: Intern, b: Bool, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("bool =");
    try indent(writer, i + 1);
    try writer.print("value = {}", .{b.value});
    try indent(writer, i + 1);
    try writer.print("type = ", .{});
    try monotype(writer, intern, b.type);
}

fn monotype(writer: List(u8).Writer, intern: Intern, m: MonoType) !void {
    switch (m) {
        .i32 => try writer.writeAll("i32"),
        .bool => try writer.writeAll("bool"),
        .void => try writer.writeAll("void"),
        .int_literal => |i| try writer.writeAll(interner.lookup(intern, i)),
        .bool_literal => |b| try writer.print("{}", .{b}),
        .typevar => |t| try writer.print("${}", .{t}),
        else => std.debug.panic("\nUnhandled monotype type {}", .{m}),
    }
}

fn if_(writer: List(u8).Writer, intern: Intern, i: If, in: Indent) !void {
    try indent(writer, in);
    try writer.writeAll("if =");
    try indent(writer, in + 1);
    try writer.writeAll("condition =");
    try expression(writer, intern, i.condition.*, in + 2);
    try indent(writer, in + 1);
    try writer.writeAll("then =");
    try block(writer, intern, i.then, in + 2);
    try indent(writer, in + 1);
    try writer.writeAll("else =");
    try block(writer, intern, i.else_, in + 2);
    try indent(writer, in + 1);
    try writer.print("type = ", .{});
    try monotype(writer, intern, i.type);
}

fn binaryOp(writer: List(u8).Writer, intern: Intern, b: BinaryOp, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("binary_op =");
    try indent(writer, i + 1);
    try writer.writeAll("kind =");
    switch (b.kind) {
        .add => try writer.writeAll("+"),
        .multiply => try writer.writeAll("*"),
        else => unreachable,
    }
    try indent(writer, i + 1);
    try writer.writeAll("left =");
    try expression(writer, intern, b.left.*, i + 2);
    try indent(writer, i + 1);
    try writer.writeAll("right =");
    try expression(writer, intern, b.right.*, i + 2);
    try indent(writer, i + 1);
    try writer.print("type = ", .{});
    try monotype(writer, intern, b.type);
}

fn expression(writer: List(u8).Writer, intern: Intern, expr: Expression, in: Indent) error{OutOfMemory}!void {
    switch (expr) {
        .symbol => |s| try symbol(writer, intern, s, in),
        .int => |i| try int(writer, intern, i, in),
        .bool => |b| try boolean(writer, intern, b, in),
        .if_ => |i| try if_(writer, intern, i, in),
        .binary_op => |b| try binaryOp(writer, intern, b, in),
        else => std.debug.panic("\nUnhandled expression type {}", .{expr}),
    }
}

fn block(writer: List(u8).Writer, intern: Intern, exprs: []const Expression, i: Indent) !void {
    std.debug.assert(exprs.len == 1);
    try expression(writer, intern, exprs[0], i);
}

fn function(writer: List(u8).Writer, intern: Intern, f: Function, i: Indent) !void {
    try indent(writer, i);
    try writer.writeAll("function");
    try indent(writer, i + 1);
    const name = interner.lookup(intern, f.name.value);
    try writer.print("name = {s}", .{name});
    try indent(writer, i + 1);
    try writer.print("parameters =", .{});
    for (f.parameters) |p| {
        try indent(writer, i + 2);
        try writer.print("parameter =", .{});
        try symbol(writer, intern, p, i + 3);
    }
    try indent(writer, i + 1);
    try writer.print("return_type = ", .{});
    try monotype(writer, intern, f.return_type);
    try indent(writer, i + 1);
    try writer.print("body =", .{});
    try block(writer, intern, f.body, i + 2);
}

fn topLevel(writer: List(u8).Writer, intern: Intern, t: TopLevel) !void {
    switch (t) {
        .function => |f| try function(writer, intern, f, 0),
        else => std.debug.panic("\nUnhandled top level type {}", .{t}),
    }
}

pub fn module(writer: List(u8).Writer, intern: Intern, m: Module) !void {
    try writer.writeAll("\n\n=== Module ===\n");
    for (m.order) |name| {
        if (m.typed.get(name)) |t| try topLevel(writer, intern, t);
    }
}

fn equal(writer: List(u8).Writer, intern: Intern, e: Equal) !void {
    try writer.print("equal = ", .{});
    try indent(writer, 1);
    try writer.print("left = ", .{});
    try monotype(writer, intern, e.left);
    try indent(writer, 1);
    try writer.print("right = ", .{});
    try monotype(writer, intern, e.right);
}

pub fn constraints(writer: List(u8).Writer, intern: Intern, c: Constraints) !void {
    try writer.writeAll("\n\n=== Constraints ===");
    for (c.equal.items) |e| {
        try writer.writeAll("\n");
        try equal(writer, intern, e);
    }
}

pub fn substitution(writer: List(u8).Writer, intern: Intern, s: Substitution) !void {
    try writer.writeAll("\n\n=== Substitution ===");
    var iterator = s.iterator();
    while (iterator.next()) |t| {
        try writer.print("\n${} = ", .{t.key_ptr.*});
        try monotype(writer, intern, t.value_ptr.*);
    }
}
