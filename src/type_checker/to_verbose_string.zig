const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const Intern = @import("../interner.zig").Intern;
const types = @import("types.zig");
const Bool = types.Bool;
const Constraints = types.Constraints;
const Module = types.Module;
const Equal = types.Equal;
const Substitution = types.Substitution;
const to_string = @import("to_string.zig");
const indent = to_string.indent;
const monotype = to_string.monotype;

pub fn module(writer: List(u8).Writer, intern: Intern, m: Module) !void {
    try writer.writeAll("\n\n=== Module ===\n");
    try to_string.toString(writer, intern, m);
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
    try writer.writeAll("\n\n=== Constraints ===");
    for (c.equal.items) |e| {
        try writer.writeAll("\n");
        try equal(writer, e);
    }
}

pub fn substitution(writer: List(u8).Writer, s: Substitution) !void {
    try writer.writeAll("\n\n=== Substitution ===");
    var iterator = s.iterator();
    while (iterator.next()) |t| {
        try writer.print("\n${} = ", .{t.key_ptr.*});
        try monotype(writer, t.value_ptr.*);
    }
}
