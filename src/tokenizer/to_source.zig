const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Tuple = std.meta.Tuple;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const Builtins = @import("../builtins.zig").Builtins;
const types = @import("types.zig");
const Span = types.Span;
const Pos = types.Pos;
const Indent = types.Indent;
const Token = types.Token;

fn repeat(writer: List(u8).Writer, c: u8, times: u64) !void {
    var i: u64 = 0;
    while (i < times) : (i += 1) try writer.writeByte(c);
}

fn space(writer: List(u8).Writer, span: Span, pos: *Pos) !void {
    try repeat(writer, ' ', span.begin.column - pos.column);
    pos.* = span.end;
}

fn indent(writer: List(u8).Writer, span: Span, i: Indent) !void {
    try repeat(writer, '\n', span.end.line - span.begin.line);
    switch (i) {
        .space => |s| try repeat(writer, ' ', s),
        .tab => |tab| try repeat(writer, '\t', tab),
    }
}

fn interned(writer: List(u8).Writer, intern: Intern, s: Interned) !void {
    try writer.writeAll(interner.lookup(intern, s));
}

pub fn toSource(allocator: Allocator, intern: Intern, tokens: []const Token) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    var pos = Pos{ .line = 1, .column = 1 };
    for (tokens) |token| {
        try space(writer, token.span, &pos);
        switch (token.kind) {
            .symbol, .int, .float => |s| try interned(writer, intern, s),
            .bool => |b| try writer.writeAll(if (b) "true" else "false"),
            .indent => |in| try indent(writer, token.span, in),
            .equal => try writer.writeAll("="),
            .dot => try writer.writeAll("."),
            .colon => try writer.writeAll(":"),
            .plus => try writer.writeAll("+"),
            .minus => try writer.writeAll("-"),
            .times => try writer.writeAll("*"),
            .caret => try writer.writeAll("^"),
            .greater => try writer.writeAll(">"),
            .less => try writer.writeAll("<"),
            .left_paren => try writer.writeAll("("),
            .right_paren => try writer.writeAll(")"),
            .if_ => try writer.writeAll("if"),
            .then => try writer.writeAll("then"),
            .else_ => try writer.writeAll("else"),
            .comma => try writer.writeAll(","),
            .arrow => try writer.writeAll("->"),
            .import => try writer.writeAll("import"),
            .export_ => try writer.writeAll("export"),
        }
    }
    return list.toOwnedSlice();
}
