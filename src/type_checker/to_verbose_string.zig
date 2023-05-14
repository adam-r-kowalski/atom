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
const MonoType = types.MonoType;
const Expression = types.Expression;
const Constraints = types.Constraints;
const Equal = types.Equal;
const Substitution = types.Substitution;

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
    try monotype(writer, s.type);
}

fn monotype(writer: List(u8).Writer, m: MonoType) !void {
    switch (m) {
        .i32 => try writer.print("i32", .{}),
        .void => try writer.print("void", .{}),
        .typevar => |t| try writer.print("${}", .{t}),
        else => std.debug.panic("\nUnhandled monotype type {}", .{m}),
    }
}

fn expression(writer: List(u8).Writer, intern: Intern, expr: Expression, i: Indent) !void {
    switch (expr) {
        .symbol => |s| try symbol(writer, intern, s, i),
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
    try monotype(writer, f.return_type);
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

fn equal(writer: List(u8).Writer, e: Equal) !void {
    try writer.print("equal = ", .{});
    try indent(writer, 1);
    try writer.print("left = ", .{});
    try monotype(writer, e.left);
    try indent(writer, 1);
    try writer.print("right = ", .{});
    try monotype(writer, e.right);
}

pub fn constraints(writer: List(u8).Writer, c: Constraints) !void {
    try writer.writeAll("\n\n=== Constraints ===\n");
    for (c.equal.items) |e| try equal(writer, e);
}

pub fn substitution(writer: List(u8).Writer, s: Substitution) !void {
    try writer.writeAll("\n\n=== Substitution ===\n");
    var iterator = s.iterator();
    while (iterator.next()) |t| {
        try writer.print("${} = ", .{t.key_ptr.*});
        try monotype(writer, t.value_ptr.*);
    }
}
