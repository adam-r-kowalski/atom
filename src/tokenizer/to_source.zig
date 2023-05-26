const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const Builtins = @import("../builtins.zig").Builtins;
const types = @import("types.zig");
const Span = types.Span;
const Pos = types.Pos;
const Token = types.Token;
const tokenSpan = @import("span.zig").span;

fn lines(writer: List(u8).Writer, s: Span, pos: Pos) !void {
    const delta = s.end.line - pos.line;
    for (0..delta) |_| try writer.writeAll("\n");
}

fn columns(writer: List(u8).Writer, s: Span, pos: Pos) !void {
    const delta = s.begin.column - pos.column;
    for (0..delta) |_| try writer.writeAll(" ");
}

fn span(writer: List(u8).Writer, s: Span, pos: Pos) !void {
    try lines(writer, s, pos);
    try columns(writer, s, pos);
}

pub fn toSource(allocator: Allocator, intern: Intern, tokens: []const Token) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    var pos = Pos{ .line = 1, .column = 1 };
    for (tokens) |token| {
        const current_span = tokenSpan(token);
        try span(writer, current_span, pos);
        pos = current_span.end;
        switch (token) {
            .symbol => |s| try writer.writeAll(interner.lookup(intern, s.value)),
            .int => |i| try writer.writeAll(interner.lookup(intern, i.value)),
            .float => |f| try writer.writeAll(interner.lookup(intern, f.value)),
            .string => |s| try writer.writeAll(interner.lookup(intern, s.value)),
            .bool => |b| try writer.print("{}", .{b.value}),
            .equal => try writer.writeAll("="),
            .equal_equal => try writer.writeAll("=="),
            .dot => try writer.writeAll("."),
            .colon => try writer.writeAll(":"),
            .plus => try writer.writeAll("+"),
            .minus => try writer.writeAll("-"),
            .times => try writer.writeAll("*"),
            .slash => try writer.writeAll("/"),
            .percent => try writer.writeAll("%"),
            .caret => try writer.writeAll("^"),
            .greater => try writer.writeAll(">"),
            .less => try writer.writeAll("<"),
            .left_paren => try writer.writeAll("("),
            .right_paren => try writer.writeAll(")"),
            .left_brace => try writer.writeAll("{"),
            .right_brace => try writer.writeAll("}"),
            .if_ => try writer.writeAll("if"),
            .else_ => try writer.writeAll("else"),
            .or_ => try writer.writeAll("or"),
            .comma => try writer.writeAll(","),
            .fn_ => try writer.writeAll("fn"),
            .new_line => {},
        }
    }
    return list.toOwnedSlice();
}
