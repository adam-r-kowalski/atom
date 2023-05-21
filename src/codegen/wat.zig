const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const types = @import("../lower/types.zig");
const IR = types.IR;
const Function = types.Function;
const Export = types.Export;
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
        .f32 => try writer.writeAll("f32"),
    }
}

fn i32Const(writer: List(u8).Writer, intern: Intern, interned: Interned) !void {
    const value = interner.lookup(intern, interned);
    try writer.print("(i32.const {s})", .{value});
}

fn f32Const(writer: List(u8).Writer, intern: Intern, interned: Interned) !void {
    const value = interner.lookup(intern, interned);
    try writer.print("(f32.const {s})", .{value});
}

fn block(writer: List(u8).Writer, intern: Intern, exprs: []const Expression, i: Indent) !void {
    for (exprs) |expr| {
        try indent(writer, i);
        try expression(writer, intern, expr, i);
    }
}

fn expression(writer: List(u8).Writer, intern: Intern, expr: Expression, i: Indent) error{OutOfMemory}!void {
    switch (expr) {
        .i32_const => |interned| try i32Const(writer, intern, interned),
        .f32_const => |interned| try f32Const(writer, intern, interned),
        .block => |b| try block(writer, intern, b, i),
    }
}

fn function(writer: List(u8).Writer, intern: Intern, f: Function, i: Indent) !void {
    try writer.writeAll("\n");
    try indent(writer, i);
    const name = interner.lookup(intern, f.name);
    try writer.print("(func ${s}", .{name});
    try writer.writeAll(" (result ");
    try typeString(writer, f.return_type);
    try writer.writeAll(")");
    try expression(writer, intern, f.body.*, i + 1);
    try writer.writeAll(")");
}

fn export_(writer: List(u8).Writer, intern: Intern, e: Export) !void {
    try writer.writeAll("\n");
    try indent(writer, 1);
    switch (e) {
        .function => |f| {
            const alias = interner.lookup(intern, f.alias);
            const name = interner.lookup(intern, f.name);
            try writer.print("(export \"{s}\" (func ${s}))", .{ alias, name });
        },
    }
}

pub fn wat(allocator: Allocator, intern: Intern, ir: IR) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    try writer.writeAll("(module");
    for (ir.functions) |f| try function(writer, intern, f, 1);
    for (ir.exports) |e| try export_(writer, intern, e);
    try writer.writeAll(")");
    return list.toOwnedSlice();
}
