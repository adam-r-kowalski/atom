const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const types = @import("types.zig");
const IR = types.IR;
const Function = types.Function;
const Expression = types.Expression;
const Type = types.Type;
const interner = @import("../interner.zig");
const Intern = interner.Intern;

fn typeString(writer: List(u8).Writer, t: Type) !void {
    switch (t) {
        .i32 => try writer.writeAll("i32"),
    }
}

fn expression(writer: List(u8).Writer, e: Expression) !void {
    switch (e) {
        .i32 => |i| try writer.print("\n    i32 {d}", .{i}),
    }
}

fn function(writer: List(u8).Writer, intern: Intern, f: Function) !void {
    const name = interner.lookup(intern, f.name);
    try writer.print("fn {s}(", .{name});
    for (f.parameters) |p| {
        const parameter_name = interner.lookup(intern, p.name);
        try writer.print("{s}: ", .{parameter_name});
        try typeString(writer, p.type);
    }
    try writer.writeAll(") ");
    try typeString(writer, f.return_type);
    try writer.writeAll(" =");
    for (f.body) |e| try expression(writer, e);
}

pub fn toString(allocator: Allocator, intern: Intern, ir: IR) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    for (ir.functions) |f| try function(writer, intern, f);
    return list.toOwnedSlice();
}
