const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Tuple = std.meta.Tuple;

const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;

pub const Indent = union(enum) {
    space: u8,
    tab: u8,
};

const Kind = union(enum) {
    symbol: Interned,
    int: Interned,
    float: Interned,
    equal,
    backslash,
    dot,
    colon,
    plus,
    minus,
    arrow,
    left_paren,
    right_paren,
};

const Pos = struct {
    line: usize,
    column: usize,
};

pub const Span = struct {
    begin: Pos,
    end: Pos,
};

const Cursor = struct {
    source: []const u8,
    pos: Pos,
};

pub const Tokens = struct {
    kind: List(Kind),
    span: List(Span),

    pub fn deinit(self: Tokens) void {
        self.kind.deinit();
        self.span.deinit();
    }
};

fn trim(cursor: *Cursor) void {
    var i: usize = 0;
    while (i < cursor.source.len and cursor.source[i] == ' ') : (i += 1) {}
    cursor.pos.column += i;
    cursor.source = cursor.source[i..];
}

fn reserved(c: u8) bool {
    return switch (c) {
        ' ', '\n', '(', ')', '.', ':' => true,
        else => false,
    };
}

fn advance(cursor: *Cursor, n: usize) []const u8 {
    const value = cursor.source[0..n];
    cursor.source = cursor.source[n..];
    cursor.pos.column += n;
    return value;
}

fn number(intern: *Intern, tokens: *Tokens, cursor: *Cursor) !void {
    const begin = cursor.pos;
    var i: usize = 1;
    var decimals: usize = if (cursor.source[0] == '.') 1 else 0;
    while (i < cursor.source.len) : (i += 1) {
        switch (cursor.source[i]) {
            '0'...'9' => {},
            '.' => decimals += 1,
            else => break,
        }
    }
    if (i > 2 and cursor.source[i - 1] == '.') {
        i -= 1;
        decimals -= 1;
    }
    const string = cursor.source[0..i];
    if (string.len == 1) {
        switch (string[0]) {
            '-' => return choice(tokens, cursor, .minus, &.{.{ '>', .arrow }}),
            '.' => {
                _ = advance(cursor, i);
                try tokens.span.append(.{ .begin = begin, .end = cursor.pos });
                try tokens.kind.append(.dot);
                return;
            },
            else => {},
        }
    }
    _ = advance(cursor, i);
    try tokens.span.append(.{ .begin = begin, .end = cursor.pos });
    const interned = try intern.string(string);
    if (decimals == 0) return try tokens.kind.append(.{ .int = interned });
    try tokens.kind.append(.{ .float = interned });
}

fn exact(tokens: *Tokens, cursor: *Cursor, kind: Kind) !void {
    const begin = cursor.pos;
    _ = advance(cursor, 1);
    try tokens.kind.append(kind);
    try tokens.span.append(.{ .begin = begin, .end = cursor.pos });
}

const Choice = Tuple(&.{ u8, Kind });

fn choice(tokens: *Tokens, cursor: *Cursor, kind: Kind, choices: []const Choice) !void {
    const begin = cursor.pos;
    if (cursor.source.len > 1) {
        const t = cursor.source[1];
        for (choices) |c| {
            if (t == c[0]) {
                _ = advance(cursor, 2);
                try tokens.span.append(.{ .begin = begin, .end = cursor.pos });
                try tokens.kind.append(c[1]);
                return;
            }
        }
    }
    _ = advance(cursor, 1);
    try tokens.span.append(.{ .begin = begin, .end = cursor.pos });
    try tokens.kind.append(kind);
}

fn symbol(tokens: *Tokens, intern: *Intern, cursor: *Cursor) !void {
    const begin = cursor.pos;
    var i: usize = 0;
    while (i < cursor.source.len and !reserved(cursor.source[i])) : (i += 1) {}
    const string = advance(cursor, i);
    const interned = try intern.string(string);
    const end = cursor.pos;
    const span = Span{ .begin = begin, .end = end };
    try tokens.kind.append(.{ .symbol = interned });
    try tokens.span.append(span);
}

pub fn tokenize(allocator: Allocator, intern: *Intern, source: []const u8) !Tokens {
    var tokens = Tokens{
        .kind = List(Kind).init(allocator),
        .span = List(Span).init(allocator),
    };
    var cursor = Cursor{
        .source = source,
        .pos = .{ .line = 1, .column = 1 },
    };
    while (cursor.source.len != 0) {
        trim(&cursor);
        switch (cursor.source[0]) {
            '0'...'9', '-', '.' => try number(intern, &tokens, &cursor),
            '=' => try exact(&tokens, &cursor, .equal),
            '\\' => try exact(&tokens, &cursor, .backslash),
            ':' => try exact(&tokens, &cursor, .colon),
            '+' => try exact(&tokens, &cursor, .plus),
            '(' => try exact(&tokens, &cursor, .left_paren),
            ')' => try exact(&tokens, &cursor, .right_paren),
            else => try symbol(&tokens, intern, &cursor),
        }
    }
    return tokens;
}

pub fn toString(allocator: Allocator, intern: Intern, tokens: Tokens) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    for (tokens.kind.items) |kind, i| {
        if (i != 0) try writer.writeAll("\n");
        switch (kind) {
            .symbol => |interned| {
                const string = intern.lookup(interned);
                try std.fmt.format(writer, "symbol {s}", .{string});
            },
            .int => |interned| {
                const string = intern.lookup(interned);
                try std.fmt.format(writer, "int {s}", .{string});
            },
            .float => |interned| {
                const string = intern.lookup(interned);
                try std.fmt.format(writer, "float {s}", .{string});
            },
            .equal => try writer.writeAll("equal"),
            .backslash => try writer.writeAll("backslash"),
            .dot => try writer.writeAll("dot"),
            .colon => try writer.writeAll("colon"),
            .plus => try writer.writeAll("plus"),
            .minus => try writer.writeAll("minus"),
            .arrow => try writer.writeAll("arrow"),
            .left_paren => try writer.writeAll("left paren"),
            .right_paren => try writer.writeAll("right paren"),
        }
    }
    return list.toOwnedSlice();
}

pub fn toSource(allocator: Allocator, intern: Intern, tokens: Tokens) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    var pos = Pos{ .line = 1, .column = 1 };
    for (tokens.kind.items) |kind, i| {
        const span = tokens.span.items[i];
        const delta = span.begin.column - pos.column;
        var j: usize = 0;
        while (j < delta) : (j += 1) try writer.writeAll(" ");
        pos.column = span.end.column;
        switch (kind) {
            .symbol, .int, .float => |interned| {
                const string = intern.lookup(interned);
                try writer.writeAll(string);
            },
            .equal => try writer.writeAll("="),
            .backslash => try writer.writeAll("\\"),
            .dot => try writer.writeAll("."),
            .colon => try writer.writeAll(":"),
            .plus => try writer.writeAll("+"),
            .minus => try writer.writeAll("-"),
            .arrow => try writer.writeAll("->"),
            .left_paren => try writer.writeAll("("),
            .right_paren => try writer.writeAll(")"),
        }
    }
    return list.toOwnedSlice();
}
