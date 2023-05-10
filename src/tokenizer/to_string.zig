const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Tuple = std.meta.Tuple;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const Builtins = @import("../builtins.zig").Builtins;
const types = @import("types.zig");
const Token = types.Token;
const Span = types.Span;
const Pos = types.Pos;
const Indent = types.Indent;
const Symbol = types.Symbol;
const Int = types.Int;
const Float = types.Float;

fn symbol(writer: List(u8).Writer, intern: Intern, s: Symbol) !void {
    try std.fmt.format(writer, "symbol {s}", .{interner.lookup(intern, s.value)});
}

fn int(writer: List(u8).Writer, intern: Intern, i: Int) !void {
    try std.fmt.format(writer, "int {s}", .{interner.lookup(intern, i.value)});
}

fn float(writer: List(u8).Writer, intern: Intern, f: Float) !void {
    try std.fmt.format(writer, "float {s}", .{interner.lookup(intern, f.value)});
}

fn indent(writer: List(u8).Writer, i: Indent) !void {
    switch (i) {
        .space => |space| try std.fmt.format(writer, "space {d}", .{space.count}),
        .tab => |tab| try std.fmt.format(writer, "tab {d}", .{tab.count}),
    }
}

pub fn toString(allocator: Allocator, intern: Intern, tokens: []const Token) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    for (tokens) |token, i| {
        if (i != 0) try writer.writeAll("\n");
        switch (token) {
            .symbol => |s| try symbol(writer, intern, s),
            .int => |s| try int(writer, intern, s),
            .float => |s| try float(writer, intern, s),
            .bool => |b| try std.fmt.format(writer, "bool {}", .{b}),
            .indent => |in| try indent(writer, in),
            .equal => try writer.writeAll("equal"),
            .dot => try writer.writeAll("dot"),
            .colon => try writer.writeAll("colon"),
            .plus => try writer.writeAll("plus"),
            .minus => try writer.writeAll("minus"),
            .times => try writer.writeAll("times"),
            .caret => try writer.writeAll("caret"),
            .greater => try writer.writeAll("greater"),
            .less => try writer.writeAll("less"),
            .left_paren => try writer.writeAll("left paren"),
            .right_paren => try writer.writeAll("right paren"),
            .if_ => try writer.writeAll("if"),
            .then => try writer.writeAll("then"),
            .else_ => try writer.writeAll("else"),
            .comma => try writer.writeAll("comma"),
            .arrow => try writer.writeAll("arrow"),
            .import => try writer.writeAll("import"),
            .export_ => try writer.writeAll("export"),
        }
    }
    return list.toOwnedSlice();
}
