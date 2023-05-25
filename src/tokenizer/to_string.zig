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

fn symbol(writer: List(u8).Writer, intern: Intern, interned: Interned) !void {
    try writer.print("symbol {s}", .{interner.lookup(intern, interned)});
}

fn int(writer: List(u8).Writer, intern: Intern, interned: Interned) !void {
    try writer.print("int {s}", .{interner.lookup(intern, interned)});
}

fn float(writer: List(u8).Writer, intern: Intern, interned: Interned) !void {
    try writer.print("float {s}", .{interner.lookup(intern, interned)});
}

fn string(writer: List(u8).Writer, intern: Intern, interned: Interned) !void {
    try writer.print("string {s}", .{interner.lookup(intern, interned)});
}

pub fn toString(allocator: Allocator, intern: Intern, tokens: []const Token) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    for (tokens, 0..) |token, i| {
        if (i != 0) try writer.writeAll("\n");
        switch (token) {
            .symbol => |s| try symbol(writer, intern, s.value),
            .int => |s| try int(writer, intern, s.value),
            .float => |s| try float(writer, intern, s.value),
            .string => |s| try string(writer, intern, s.value),
            .bool => |b| try writer.print("bool {}", .{b.value}),
            .equal => try writer.writeAll("equal"),
            .equal_equal => try writer.writeAll("equal equal"),
            .dot => try writer.writeAll("dot"),
            .colon => try writer.writeAll("colon"),
            .plus => try writer.writeAll("plus"),
            .minus => try writer.writeAll("minus"),
            .times => try writer.writeAll("times"),
            .percent => try writer.writeAll("percent"),
            .caret => try writer.writeAll("caret"),
            .greater => try writer.writeAll("greater"),
            .less => try writer.writeAll("less"),
            .left_paren => try writer.writeAll("left paren"),
            .right_paren => try writer.writeAll("right paren"),
            .left_brace => try writer.writeAll("left brace"),
            .right_brace => try writer.writeAll("right brace"),
            .if_ => try writer.writeAll("if"),
            .else_ => try writer.writeAll("else"),
            .or_ => try writer.writeAll("or"),
            .comma => try writer.writeAll("comma"),
            .fn_ => try writer.writeAll("fn"),
            .new_line => try writer.writeAll("new line"),
        }
    }
    return list.toOwnedSlice();
}
