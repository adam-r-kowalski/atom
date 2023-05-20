const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Tuple = std.meta.Tuple;

const interner = @import("../interner.zig");
const Intern = interner.Intern;
const Builtins = @import("../builtins.zig").Builtins;
const types = @import("types.zig");
const Pos = types.Pos;
const Token = types.Token;
const Span = types.Span;
const Kind = types.Kind;

const Cursor = struct {
    source: []const u8,
    pos: Pos,
};

fn trim(cursor: *Cursor) void {
    var i: u64 = 0;
    while (i < cursor.source.len and cursor.source[i] == ' ') : (i += 1) {}
    cursor.pos.column += i;
    cursor.source = cursor.source[i..];
}

fn reserved(c: u8) bool {
    return switch (c) {
        ' ', '\n', '(', ')', '.', ':', ',' => true,
        else => false,
    };
}

fn advance(cursor: *Cursor, n: u64) []const u8 {
    const value = cursor.source[0..n];
    cursor.source = cursor.source[n..];
    cursor.pos.column += n;
    return value;
}

fn number(intern: *Intern, cursor: *Cursor) !Token {
    const begin = cursor.pos;
    var i: u64 = 1;
    var decimals: u64 = if (cursor.source[0] == '.') 1 else 0;
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
            '-' => return exact(cursor, .minus),
            '.' => {
                _ = advance(cursor, i);
                return Token{ .kind = .dot, .span = .{ .begin = begin, .end = cursor.pos } };
            },
            else => {},
        }
    }
    _ = advance(cursor, i);
    const span = Span{ .begin = begin, .end = cursor.pos };
    const interned = try interner.store(intern, string);
    if (decimals == 0) return Token{ .kind = .{ .int = interned }, .span = span };
    return Token{ .kind = .{ .float = interned }, .span = span };
}

fn exact(cursor: *Cursor, comptime kind: Kind) Token {
    const begin = cursor.pos;
    _ = advance(cursor, 1);
    return Token{ .kind = kind, .span = .{ .begin = begin, .end = cursor.pos } };
}

const Choice = Tuple(&.{ u8, std.meta.Tag(Token) });

fn choice(cursor: *Cursor, comptime kind: std.meta.Tag(Token), comptime choices: []const Choice) Token {
    const begin = cursor.pos;
    if (cursor.source.len > 1) {
        const t = cursor.source[1];
        inline for (choices) |c| {
            if (t == c[0]) {
                _ = advance(cursor, 2);
                return @unionInit(Token, @tagName(c[1]), .{ .span = .{ .begin = begin, .end = cursor.pos } });
            }
        }
    }
    _ = advance(cursor, 1);
    return @unionInit(Token, @tagName(kind), .{ .span = .{ .begin = begin, .end = cursor.pos } });
}

fn symbol(intern: *Intern, builtins: Builtins, cursor: *Cursor) !Token {
    const begin = cursor.pos;
    var i: u64 = 0;
    while (i < cursor.source.len and !reserved(cursor.source[i])) : (i += 1) {}
    const string = advance(cursor, i);
    const end = cursor.pos;
    const span = Span{ .begin = begin, .end = end };
    const interned = try interner.store(intern, string);
    if (interned == builtins.fn_) return Token{ .kind = .fn_, .span = span };
    if (interned == builtins.import) return Token{ .kind = .import, .span = span };
    if (interned == builtins.export_) return Token{ .kind = .export_, .span = span };
    if (interned == builtins.if_) return Token{ .kind = .if_, .span = span };
    if (interned == builtins.true_) return Token{ .kind = .{ .bool = true }, .span = span };
    if (interned == builtins.false_) return Token{ .kind = .{ .bool = false }, .span = span };
    return Token{ .kind = .{ .symbol = interned }, .span = span };
}

fn nextToken(cursor: *Cursor, intern: *Intern, builtins: Builtins) !?Token {
    trim(cursor);
    if (cursor.source.len == 0) return null;
    return switch (cursor.source[0]) {
        '0'...'9', '-', '.' => try number(intern, cursor),
        '=' => exact(cursor, .equal),
        ':' => exact(cursor, .colon),
        '+' => exact(cursor, .plus),
        '*' => exact(cursor, .times),
        '^' => exact(cursor, .caret),
        '>' => exact(cursor, .greater),
        '<' => exact(cursor, .less),
        '(' => exact(cursor, .left_paren),
        ')' => exact(cursor, .right_paren),
        '{' => exact(cursor, .left_brace),
        '}' => exact(cursor, .right_brace),
        ',' => exact(cursor, .comma),
        else => try symbol(intern, builtins, cursor),
    };
}

pub fn tokenize(allocator: Allocator, intern: *Intern, builtins: Builtins, source: []const u8) ![]const Token {
    var cursor = Cursor{
        .source = source,
        .pos = .{ .line = 1, .column = 1 },
    };
    var tokens = List(Token).init(allocator);
    while (try nextToken(&cursor, intern, builtins)) |token|
        try tokens.append(token);
    return tokens.toOwnedSlice();
}
