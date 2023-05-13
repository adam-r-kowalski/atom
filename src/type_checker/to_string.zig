const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const types = @import("types.zig");
const Module = types.Module;
const TopLevel = types.TopLevel;

fn topLevel(_: List(u8).Writer, _: Intern, _: TopLevel) !void {
    unreachable;
}

pub fn toString(allocator: Allocator, intern: Intern, module: Module) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    for (module.order) |name| {
        if (module.typed.get(name)) |t| try topLevel(writer, intern, t);
    }
    return list.toOwnedSlice();
}
