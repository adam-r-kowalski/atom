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
const Float = types.Float;
const MonoType = types.MonoType;
const Expression = types.Expression;
const If = types.If;
const BinaryOp = types.BinaryOp;
const Define = types.Define;
const TypeVar = types.TypeVar;

const Vars = Map(TypeVar, u32);
const Indent = u64;

fn indent(writer: List(u8).Writer, i: Indent) !void {
    var j: Indent = 0;
    try writer.writeAll("\n");
    while (j < i) : (j += 1) {
        try writer.writeAll("    ");
    }
}

fn symbol(writer: List(u8).Writer, intern: Intern, s: Symbol) !void {
    const name = interner.lookup(intern, s.value);
    try writer.print("{s}", .{name});
}

fn int(writer: List(u8).Writer, intern: Intern, i: Int) !void {
    const value = interner.lookup(intern, i.value);
    try writer.print("{s}", .{value});
}

fn float(writer: List(u8).Writer, intern: Intern, f: Float) !void {
    const value = interner.lookup(intern, f.value);
    try writer.print("{s}", .{value});
}

fn monotype(vars: *Vars, writer: List(u8).Writer, m: MonoType) !void {
    switch (m) {
        .i32 => try writer.print("i32", .{}),
        .f32 => try writer.print("f32", .{}),
        .bool => try writer.print("bool", .{}),
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

fn if_(vars: *Vars, writer: List(u8).Writer, intern: Intern, i: If, in: Indent) !void {
    try writer.writeAll("if ");
    try expression(vars, writer, intern, i.condition.*, in);
    try writer.writeAll(" then");
    try block(vars, writer, intern, i.then, in);
    try writer.writeAll(" else");
    try block(vars, writer, intern, i.else_, in);
}

fn binaryOp(vars: *Vars, writer: List(u8).Writer, intern: Intern, b: BinaryOp, i: Indent) !void {
    try expression(vars, writer, intern, b.left.*, i);
    try writer.writeAll(" ");
    switch (b.kind) {
        .add => try writer.writeAll("+"),
        .multiply => try writer.writeAll("*"),
        else => unreachable,
    }
    try writer.writeAll(" ");
    try expression(vars, writer, intern, b.right.*, i);
}

fn define(vars: *Vars, writer: List(u8).Writer, intern: Intern, d: Define, i: Indent) !void {
    try symbol(writer, intern, d.name);
    try writer.writeAll(": ");
    try monotype(vars, writer, d.name.type);
    try writer.writeAll(" =");
    try block(vars, writer, intern, d.body, i + 1);
}

fn expression(vars: *Vars, writer: List(u8).Writer, intern: Intern, expr: Expression, in: Indent) error{OutOfMemory}!void {
    switch (expr) {
        .symbol => |s| try symbol(writer, intern, s),
        .int => |i| try int(writer, intern, i),
        .float => |f| try float(writer, intern, f),
        .bool => |b| try writer.print("{}", .{b.value}),
        .if_ => |i| try if_(vars, writer, intern, i, in),
        .binary_op => |b| try binaryOp(vars, writer, intern, b, in),
        .define => |d| try define(vars, writer, intern, d, in),
        else => std.debug.panic("\nUnhandled expression type {}", .{expr}),
    }
}

fn block(vars: *Vars, writer: List(u8).Writer, intern: Intern, exprs: []const Expression, i: Indent) !void {
    if (exprs.len == 1) {
        try writer.writeAll(" ");
        try expression(vars, writer, intern, exprs[0], i);
        return;
    }
    for (exprs) |e| {
        try indent(writer, i);
        try expression(vars, writer, intern, e, i);
    }
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
        try monotype(&vars, sub_writer, p.type);
    }
    try sub_writer.print(") -> ", .{});
    try monotype(&vars, sub_writer, f.return_type);
    try sub_writer.print(" =", .{});
    try block(&vars, sub_writer, intern, f.body, 1);
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
