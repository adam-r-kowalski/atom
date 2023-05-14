const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const types = @import("types.zig");
const Module = types.Module;
const TopLevel = types.TopLevel;
const Function = types.Function;
const Symbol = types.Symbol;
const Int = types.Int;
const MonoType = types.MonoType;
const Expression = types.Expression;
const If = types.If;
const TypeVar = types.TypeVar;

const Vars = Map(TypeVar, u32);

fn symbol(writer: List(u8).Writer, intern: Intern, s: Symbol) !void {
    const name = interner.lookup(intern, s.value);
    try writer.print("{s}", .{name});
}

fn int(writer: List(u8).Writer, intern: Intern, i: Int) !void {
    const value = interner.lookup(intern, i.value);
    try writer.print("{s}", .{value});
}

fn monotype(vars: *Vars, writer: List(u8).Writer, intern: Intern, m: MonoType) !void {
    switch (m) {
        .i32 => try writer.print("i32", .{}),
        .f32 => try writer.print("f32", .{}),
        .bool => try writer.print("bool", .{}),
        .int_literal => |i| try writer.writeAll(interner.lookup(intern, i)),
        .bool_literal => |b| try writer.print("{}", .{b}),
        .typevar => |t| {
            const result = try vars.getOrPut(t);
            if (!result.found_existing) {
                const mapped = @as(u32, 'A') + vars.count() - 1;
                result.value_ptr.* = mapped;
            }
            try writer.print("{c}", .{@intCast(u8, result.value_ptr.*)});
        },
        else => std.debug.panic("\nUnhandled monotype type {}", .{m}),
    }
}

fn if_(writer: List(u8).Writer, intern: Intern, i: If) !void {
    try writer.writeAll("if ");
    try expression(writer, intern, i.condition.*);
    try writer.writeAll(" then ");
    try block(writer, intern, i.then);
    try writer.writeAll(" else ");
    try block(writer, intern, i.else_);
}

fn expression(writer: List(u8).Writer, intern: Intern, expr: Expression) error{OutOfMemory}!void {
    switch (expr) {
        .symbol => |s| try symbol(writer, intern, s),
        .int => |i| try int(writer, intern, i),
        .bool => |b| try writer.print("{}", .{b.value}),
        .if_ => |i| try if_(writer, intern, i),
        else => std.debug.panic("\nUnhandled expression type {}", .{expr}),
    }
}

fn block(writer: List(u8).Writer, intern: Intern, exprs: []const Expression) !void {
    std.debug.assert(exprs.len == 1);
    try expression(writer, intern, exprs[0]);
}

fn function(allocator: Allocator, writer: List(u8).Writer, intern: Intern, f: Function) !void {
    const name = interner.lookup(intern, f.name.value);
    try writer.print("{s}", .{name});
    var vars = Vars.init(allocator);
    defer vars.deinit();
    var list = List(u8).init(allocator);
    defer list.deinit();
    const sub_writer = list.writer();
    try sub_writer.writeAll("(");
    for (f.parameters) |p, i| {
        if (i > 0) try sub_writer.writeAll(", ");
        try symbol(sub_writer, intern, p);
        try sub_writer.writeAll(": ");
        try monotype(&vars, sub_writer, intern, p.type);
    }
    try sub_writer.print(") -> ", .{});
    try monotype(&vars, sub_writer, intern, f.return_type);
    try sub_writer.print(" = ", .{});
    try block(sub_writer, intern, f.body);
    const var_count = vars.count();
    if (var_count > 0) {
        try writer.writeAll("[");
        var i: u32 = 0;
        while (i < var_count) : (i += 1) {
            try writer.print("{c}", .{'A' + @intCast(u8, i)});
        }
        try writer.writeAll("]");
    }
    try writer.writeAll(list.items);
}

fn topLevel(allocator: Allocator, writer: List(u8).Writer, intern: Intern, t: TopLevel) !void {
    switch (t) {
        .function => |f| try function(allocator, writer, intern, f),
        else => std.debug.panic("\nUnhandled top level type {}", .{t}),
    }
}

pub fn toString(allocator: Allocator, intern: Intern, module: Module) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    for (module.order) |name| {
        if (module.typed.get(name)) |t| try topLevel(allocator, writer, intern, t);
    }
    return list.toOwnedSlice();
}
