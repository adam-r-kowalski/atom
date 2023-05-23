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
const BinaryOp = types.BinaryOp;
const Call = types.Call;
const If = types.If;
const LocalSet = types.LocalSet;

const Indent = u64;

pub fn indent(writer: List(u8).Writer, n: Indent) !void {
    try writer.writeAll("\n");
    for (0..n) |_| try writer.writeAll("    ");
}

fn typeString(writer: List(u8).Writer, t: Type) !void {
    switch (t) {
        .i32 => try writer.writeAll("i32"),
        .f32 => try writer.writeAll("f32"),
    }
}

fn localGet(writer: List(u8).Writer, intern: Intern, interned: Interned) !void {
    const value = interner.lookup(intern, interned);
    try writer.print("(local.get ${s})", .{value});
}

fn localSet(writer: List(u8).Writer, intern: Intern, local_set: LocalSet, i: Indent) !void {
    const value = interner.lookup(intern, local_set.name);
    try writer.print("(local.set ${s}", .{value});
    try indent(writer, i);
    try expression(writer, intern, local_set.value.*, i);
    try writer.writeAll(")");
}

fn i32Const(writer: List(u8).Writer, intern: Intern, interned: Interned) !void {
    const value = interner.lookup(intern, interned);
    try writer.print("(i32.const {s})", .{value});
}

fn f32Const(writer: List(u8).Writer, intern: Intern, interned: Interned) !void {
    const value = interner.lookup(intern, interned);
    try writer.print("(f32.const {s})", .{value});
}

fn binaryOp(writer: List(u8).Writer, intern: Intern, op: []const u8, b: BinaryOp, i: Indent) !void {
    try writer.print("({s}", .{op});
    try indent(writer, i);
    try expression(writer, intern, b.left.*, i);
    try indent(writer, i);
    try expression(writer, intern, b.right.*, i);
    try writer.writeAll(")");
}

fn block(writer: List(u8).Writer, intern: Intern, exprs: []const Expression, i: Indent) !void {
    for (exprs) |expr| {
        try indent(writer, i);
        try expression(writer, intern, expr, i);
    }
}

fn call(writer: List(u8).Writer, intern: Intern, c: Call, i: Indent) !void {
    const name = interner.lookup(intern, c.function);
    try writer.print("(call ${s}", .{name});
    for (c.arguments) |arg| {
        try indent(writer, i);
        try expression(writer, intern, arg, i);
    }
    try writer.writeAll(")");
}

fn conditional(writer: List(u8).Writer, intern: Intern, c: If, i: Indent) !void {
    try writer.writeAll("(if (result ");
    try typeString(writer, c.result);
    try writer.writeAll(")");
    try indent(writer, i);
    try expression(writer, intern, c.condition.*, i);
    try indent(writer, i);
    try writer.writeAll("(then");
    try expression(writer, intern, c.then.*, i + 1);
    try writer.writeAll(")");
    try indent(writer, i);
    try writer.writeAll("(else");
    try expression(writer, intern, c.else_.*, i + 1);
    try writer.writeAll("))");
}

fn expression(writer: List(u8).Writer, intern: Intern, expr: Expression, i: Indent) error{OutOfMemory}!void {
    switch (expr) {
        .local_get => |interned| try localGet(writer, intern, interned),
        .local_set => |local_set| try localSet(writer, intern, local_set, i + 1),
        .i32_const => |interned| try i32Const(writer, intern, interned),
        .i32_add => |b| try binaryOp(writer, intern, "i32.add", b, i + 1),
        .i32_sub => |b| try binaryOp(writer, intern, "i32.sub", b, i + 1),
        .i32_mul => |b| try binaryOp(writer, intern, "i32.mul", b, i + 1),
        .i32_eq => |b| try binaryOp(writer, intern, "i32.eq", b, i + 1),
        .i32_rem_s => |b| try binaryOp(writer, intern, "i32.rem_s", b, i + 1),
        .i32_or => |b| try binaryOp(writer, intern, "i32.or", b, i + 1),
        .i32_gt_s => |b| try binaryOp(writer, intern, "i32.gt_s", b, i + 1),
        .f32_const => |interned| try f32Const(writer, intern, interned),
        .f32_add => |b| try binaryOp(writer, intern, "f32.add", b, i + 1),
        .f32_sub => |b| try binaryOp(writer, intern, "f32.sub", b, i + 1),
        .f32_mul => |b| try binaryOp(writer, intern, "f32.mul", b, i + 1),
        .f32_eq => |b| try binaryOp(writer, intern, "f32.eq", b, i + 1),
        .f32_gt => |b| try binaryOp(writer, intern, "f32.gt", b, i + 1),
        .block => |b| try block(writer, intern, b, i),
        .call => |c| try call(writer, intern, c, i + 1),
        .if_ => |c| try conditional(writer, intern, c, i + 1),
    }
}

fn function(writer: List(u8).Writer, intern: Intern, f: Function, i: Indent) !void {
    try writer.writeAll("\n");
    try indent(writer, i);
    const name = interner.lookup(intern, f.name);
    try writer.print("(func ${s}", .{name});
    for (f.parameters) |p| {
        const name_symbol = interner.lookup(intern, p.name);
        try writer.print(" (param ${s} ", .{name_symbol});
        try typeString(writer, p.type);
        try writer.writeAll(")");
    }
    try writer.writeAll(" (result ");
    try typeString(writer, f.return_type);
    try writer.writeAll(")");
    for (f.locals) |l| {
        const name_symbol = interner.lookup(intern, l.name);
        try indent(writer, i + 1);
        try writer.print("(local ${s} ", .{name_symbol});
        try typeString(writer, l.type);
        try writer.writeAll(")");
    }
    try expression(writer, intern, f.body.*, i + 1);
    try writer.writeAll(")");
}

fn foreignExport(writer: List(u8).Writer, intern: Intern, e: Export) !void {
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
    for (ir.exports) |e| try foreignExport(writer, intern, e);
    try writer.writeAll(")");
    return list.toOwnedSlice();
}
