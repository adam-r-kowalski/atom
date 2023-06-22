const std = @import("std");
const List = std.ArrayList;
const Writer = List(u8).Writer;

const types = @import("types.zig");
const spanOf = @import("span.zig").token;

pub fn token(t: types.Token, writer: Writer) !void {
    switch (t) {
        .comment => |c| try writer.print("{}", .{c.value}),
        .symbol => |s| try writer.print("{}", .{s.value}),
        .int => |i| try writer.print("{}", .{i.value}),
        .float => |f| try writer.print("{}", .{f.value}),
        .string => |s| try writer.print("{}", .{s.value}),
        .bool => |b| try writer.print("{}", .{b.value}),
        .equal => try writer.writeAll("="),
        .equal_equal => try writer.writeAll("=="),
        .dot => try writer.writeAll("."),
        .colon => try writer.writeAll(":"),
        .plus => try writer.writeAll("+"),
        .plus_equal => try writer.writeAll("+="),
        .minus => try writer.writeAll("-"),
        .times => try writer.writeAll("*"),
        .times_equal => try writer.writeAll("*="),
        .slash => try writer.writeAll("/"),
        .percent => try writer.writeAll("%"),
        .caret => try writer.writeAll("^"),
        .greater => try writer.writeAll(">"),
        .less => try writer.writeAll("<"),
        .left_paren => try writer.writeAll("("),
        .right_paren => try writer.writeAll(")"),
        .left_brace => try writer.writeAll("{"),
        .right_brace => try writer.writeAll("}"),
        .left_bracket => try writer.writeAll("["),
        .right_bracket => try writer.writeAll("]"),
        .if_ => try writer.writeAll("if"),
        .else_ => try writer.writeAll("else"),
        .or_ => try writer.writeAll("or"),
        .comma => try writer.writeAll(","),
        .fn_ => try writer.writeAll("fn"),
        .mut => try writer.writeAll("mut"),
        .undefined => try writer.writeAll("undefined"),
        .new_line => {},
    }
}

pub fn tokens(ts: []const types.Token, writer: Writer) !void {
    var pos = types.Pos{ .line = 1, .column = 1 };
    for (ts) |t| {
        const current_span = spanOf(t);
        const delta_lines = current_span.end.line - pos.line;
        for (0..delta_lines) |_| try writer.writeAll("\n");
        const delta_columns = current_span.begin.column - pos.column;
        for (0..delta_columns) |_| try writer.writeAll(" ");
        pos = current_span.end;
        try token(t, writer);
    }
}
