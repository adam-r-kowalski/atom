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

fn symbol(writer: List(u8).Writer, intern: Intern, s: Symbol) !void {
    const name = interner.lookup(intern, s.value);
    try writer.print("{s}", .{name});
}

fn monotype(writer: List(u8).Writer, m: MonoType) !void {
    switch (m) {
        .i32 => try writer.print("i32", .{}),
        .typevar => |t| try writer.print("${}", .{t}),
        else => std.debug.panic("\nUnhandled monotype type {}", .{m}),
    }
}

fn expression(writer: List(u8).Writer, intern: Intern, expr: Expression) !void {
    switch (expr) {
        .symbol => |s| try symbol(writer, intern, s),
        else => std.debug.panic("\nUnhandled expression type {}", .{expr}),
    }
}

fn block(writer: List(u8).Writer, intern: Intern, exprs: []const Expression) !void {
    std.debug.assert(exprs.len == 1);
    try expression(writer, intern, exprs[0]);
}

fn function(writer: List(u8).Writer, intern: Intern, f: Function) !void {
    const name = interner.lookup(intern, f.name.value);
    try writer.print("{s}(", .{name});
    for (f.parameters) |p| {
        try symbol(writer, intern, p);
        try writer.writeAll(": ");
        try monotype(writer, p.type);
    }
    try writer.print(") -> ", .{});
    try monotype(writer, f.return_type);
    try writer.print(" = ", .{});
    try block(writer, intern, f.body);
}

fn topLevel(writer: List(u8).Writer, intern: Intern, t: TopLevel) !void {
    switch (t) {
        .function => |f| try function(writer, intern, f),
        else => std.debug.panic("\nUnhandled top level type {}", .{t}),
    }
}

pub fn toString(allocator: Allocator, intern: Intern, module: Module) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    for (module.order) |name| {
        if (module.typed.get(name)) |t| try topLevel(writer, intern, t);
    }
    return list.toOwnedSlice();
}
