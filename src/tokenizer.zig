const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;

const Kind = union(enum) {
    symbol: Interned,
    equal,
    backslash,
    dot,
    plus,
};

const Pos = struct {
    line: usize,
    column: usize,
};

const Span = struct {
    start: Pos,
    end: Pos,
};

const Cursor = struct {
    source: []const u8,
    pos: Pos,
};

pub const Tokens = struct {
    kind: List(Kind),
    span: List(Span),

    const Self = @This();

    pub fn deinit(self: Self) void {
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
        ' ', '\n', '.', ':' => true,
        else => false,
    };
}

fn advance(cursor: *Cursor, n: usize) []const u8 {
    const value = cursor.source[0..n];
    cursor.source = cursor.source[n..];
    cursor.pos.column += n;
    return value;
}

fn exact(tokens: *Tokens, cursor: *Cursor, kind: Kind) !void {
    const start = cursor.pos;
    _ = advance(cursor, 1);
    try tokens.kind.append(kind);
    try tokens.span.append(.{ .start = start, .end = cursor.pos });
}

fn symbol(tokens: *Tokens, intern: *Intern, cursor: *Cursor) !void {
    const start = cursor.pos;
    var i: usize = 0;
    while (i < cursor.source.len and !reserved(cursor.source[i])) : (i += 1) {}
    const string = advance(cursor, i);
    const interned = try intern.string(string);
    const end = cursor.pos;
    const span = Span{ .start = start, .end = end };
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
            '=' => try exact(&tokens, &cursor, .equal),
            '\\' => try exact(&tokens, &cursor, .backslash),
            '.' => try exact(&tokens, &cursor, .dot),
            '+' => try exact(&tokens, &cursor, .plus),
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
            .equal => try writer.writeAll("equal"),
            .backslash => try writer.writeAll("backslash"),
            .dot => try writer.writeAll("dot"),
            .plus => try writer.writeAll("plus"),
        }
    }
    return list.toOwnedSlice();
}
