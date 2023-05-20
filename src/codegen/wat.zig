const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const types = @import("../lower/types.zig");
const IR = types.IR;
const Function = types.Function;
const Type = types.Type;
const Expression = types.Expression;

const Indent = u64;

pub fn indent(writer: List(u8).Writer, n: Indent) !void {
    try writer.writeAll("\n");
    var i: u64 = 0;
    while (i < n) : (i += 1) try writer.writeAll("    ");
}

fn typeString(writer: List(u8).Writer, t: Type) !void {
    switch (t) {
        .i32 => try writer.writeAll("i32"),
    }
}

fn expression(writer: List(u8).Writer, expr: Expression) !void {
    switch (expr) {
        .i32 => |int| try writer.print("(i32.const {d})", .{int}),
    }
}

fn block(writer: List(u8).Writer, exprs: []const Expression, i: Indent) !void {
    for (exprs) |expr| {
        try indent(writer, i);
        try expression(writer, expr);
    }
}

fn function(writer: List(u8).Writer, intern: Intern, f: Function) !void {
    try indent(writer, 1);
    const name = interner.lookup(intern, f.name);
    try writer.print("(func ${s}", .{name});
    try writer.writeAll(" (result ");
    try typeString(writer, f.return_type);
    try writer.writeAll(")");
    try block(writer, f.body, 2);
    try writer.writeAll(")");
}

pub fn wat(allocator: Allocator, intern: Intern, ir: IR) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    try writer.writeAll("(module");
    for (ir.functions) |f| try function(writer, intern, f);
    try writer.writeAll(")");
    return list.toOwnedSlice();
}
