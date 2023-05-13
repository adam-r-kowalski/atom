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
const Symbol = types.Symbol;
const Int = types.Int;
const Float = types.Float;

fn repeat(writer: List(u8).Writer, c: u8, times: u64) !void {
    var i: u64 = 0;
    while (i < times) : (i += 1) try writer.writeByte(c);
}

fn space(writer: List(u8).Writer, span: Span, pos: *Pos) !void {
    try repeat(writer, ' ', span.begin.column - pos.column);
    pos.* = span.end;
}

fn indent(writer: List(u8).Writer, i: Indent, pos: *Pos) !void {
    try space(writer, i.span, pos);
    try repeat(writer, '\n', i.span.end.line - i.span.begin.line);
    try repeat(writer, if (i.kind == .space) ' ' else '\t', i.count);
}

fn symbol(writer: List(u8).Writer, intern: Intern, s: Symbol, pos: *Pos) !void {
    try space(writer, s.span, pos);
    try writer.writeAll(interner.lookup(intern, s.value));
}

fn int(writer: List(u8).Writer, intern: Intern, i: Int, pos: *Pos) !void {
    try space(writer, i.span, pos);
    try writer.writeAll(interner.lookup(intern, i.value));
}

fn float(writer: List(u8).Writer, intern: Intern, f: Float, pos: *Pos) !void {
    try space(writer, f.span, pos);
    try writer.writeAll(interner.lookup(intern, f.value));
}

fn write(writer: List(u8).Writer, span: Span, pos: *Pos, s: []const u8) !void {
    try space(writer, span, pos);
    try writer.writeAll(s);
}

pub fn toSource(allocator: Allocator, intern: Intern, tokens: []const Token) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    var pos = Pos{ .line = 1, .column = 1 };
    for (tokens) |token| {
        switch (token) {
            .symbol => |s| try symbol(writer, intern, s, &pos),
            .int => |i| try int(writer, intern, i, &pos),
            .float => |f| try float(writer, intern, f, &pos),
            .bool => |b| try write(writer, b.span, &pos, if (b.value) "true" else "false"),
            .indent => |i| try indent(writer, i, &pos),
            .equal => |e| try write(writer, e.span, &pos, "="),
            .dot => |d| try write(writer, d.span, &pos, "."),
            .colon => |c| try write(writer, c.span, &pos, ":"),
            .plus => |p| try write(writer, p.span, &pos, "+"),
            .minus => |m| try write(writer, m.span, &pos, "-"),
            .times => |t| try write(writer, t.span, &pos, "*"),
            .caret => |c| try write(writer, c.span, &pos, "^"),
            .greater => |g| try write(writer, g.span, &pos, ">"),
            .less => |l| try write(writer, l.span, &pos, "<"),
            .left_paren => |l| try write(writer, l.span, &pos, "("),
            .right_paren => |r| try write(writer, r.span, &pos, ")"),
            .if_ => |i| try write(writer, i.span, &pos, "if"),
            .then => |t| try write(writer, t.span, &pos, "then"),
            .else_ => |e| try write(writer, e.span, &pos, "else"),
            .comma => |c| try write(writer, c.span, &pos, ","),
            .arrow => |a| try write(writer, a.span, &pos, "->"),
            .import => |i| try write(writer, i.span, &pos, "import"),
            .export_ => |e| try write(writer, e.span, &pos, "export"),
        }
    }
    return list.toOwnedSlice();
}
