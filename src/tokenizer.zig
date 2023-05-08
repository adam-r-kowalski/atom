const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Tuple = std.meta.Tuple;

const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;
const Builtins = @import("builtins.zig").Builtins;

pub const Indent = union(enum) {
    space: u64,
    tab: u64,
};

pub const Kind = union(enum) {
    symbol: Interned,
    int: Interned,
    float: Interned,
    indent: Indent,
    bool: bool,
    equal,
    dot,
    colon,
    plus,
    minus,
    times,
    caret,
    greater,
    less,
    left_paren,
    right_paren,
    if_,
    then,
    else_,
    comma,
    arrow,
};

const Pos = struct {
    line: u64,
    column: u64,
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

fn number(intern: *Intern, tokens: *Tokens, cursor: *Cursor) !void {
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
    const interned = try interner.store(intern, string);
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

fn symbol(tokens: *Tokens, intern: *Intern, builtins: Builtins, cursor: *Cursor) !void {
    const begin = cursor.pos;
    var i: u64 = 0;
    while (i < cursor.source.len and !reserved(cursor.source[i])) : (i += 1) {}
    const string = advance(cursor, i);
    const end = cursor.pos;
    const span = Span{ .begin = begin, .end = end };
    try tokens.span.append(span);
    const interned = try interner.store(intern, string);
    if (interned == builtins.if_) return try tokens.kind.append(.if_);
    if (interned == builtins.then) return try tokens.kind.append(.then);
    if (interned == builtins.else_) return try tokens.kind.append(.else_);
    if (interned == builtins.true_) return try tokens.kind.append(.{ .bool = true });
    if (interned == builtins.false_) return try tokens.kind.append(.{ .bool = false });
    try tokens.kind.append(.{ .symbol = interned });
}

fn newLine(tokens: *Tokens, cursor: *Cursor) !void {
    var begin = cursor.pos;
    cursor.pos.column = 1;
    var i: u64 = 0;
    while (cursor.source.len > i and cursor.source[i] == '\n') : (i += 1) {}
    cursor.pos.line += i;
    cursor.source = cursor.source[i..];
    if (cursor.source.len == 0) return;
    i = 0;
    if (cursor.source[0] == ' ') {
        while (cursor.source.len > i and cursor.source[i] == ' ') : (i += 1) {}
        cursor.pos.column += i;
        cursor.source = cursor.source[i..];
        if (cursor.source.len == 0) return;
        try tokens.kind.append(.{ .indent = .{ .space = i } });
        try tokens.span.append(.{ .begin = begin, .end = cursor.pos });
        return;
    }
    if (cursor.source[0] == '\t') {
        while (cursor.source.len > i and cursor.source[i] == '\t') : (i += 1) {}
        cursor.pos.column += i;
        cursor.source = cursor.source[i..];
        if (cursor.source.len == 0) return;
        try tokens.kind.append(.{ .indent = .{ .tab = i } });
        try tokens.span.append(.{ .begin = begin, .end = cursor.pos });
        return;
    }
    try tokens.kind.append(.{ .indent = .{ .space = 0 } });
    try tokens.span.append(.{ .begin = begin, .end = cursor.pos });
}

pub fn tokenize(allocator: Allocator, intern: *Intern, builtins: Builtins, source: []const u8) !Tokens {
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
            ':' => try exact(&tokens, &cursor, .colon),
            '+' => try exact(&tokens, &cursor, .plus),
            '*' => try exact(&tokens, &cursor, .times),
            '^' => try exact(&tokens, &cursor, .caret),
            '>' => try exact(&tokens, &cursor, .greater),
            '<' => try exact(&tokens, &cursor, .less),
            '(' => try exact(&tokens, &cursor, .left_paren),
            ')' => try exact(&tokens, &cursor, .right_paren),
            ',' => try exact(&tokens, &cursor, .comma),
            '\n' => try newLine(&tokens, &cursor),
            else => try symbol(&tokens, intern, builtins, &cursor),
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
                const string = interner.lookup(intern, interned);
                try std.fmt.format(writer, "symbol {s}", .{string});
            },
            .int => |interned| {
                const string = interner.lookup(intern, interned);
                try std.fmt.format(writer, "int {s}", .{string});
            },
            .float => |interned| {
                const string = interner.lookup(intern, interned);
                try std.fmt.format(writer, "float {s}", .{string});
            },
            .bool => |b| try std.fmt.format(writer, "bool {}", .{b}),
            .indent => |indent| {
                switch (indent) {
                    .space => try std.fmt.format(writer, "space {d}", .{indent.space}),
                    .tab => try std.fmt.format(writer, "tab {d}", .{indent.tab}),
                }
            },
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
        }
    }
    return list.toOwnedSlice();
}

fn spaceToSource(writer: List(u8).Writer, span: Span, pos: *Pos) !void {
    const delta = span.begin.column - pos.column;
    var i: u64 = 0;
    while (i < delta) : (i += 1) try writer.writeAll(" ");
    pos.* = span.end;
}

fn indentToSource(writer: List(u8).Writer, span: Span, indent: Indent) !void {
    const delta = span.end.line - span.begin.line;
    {
        var i: u64 = 0;
        while (i < delta) : (i += 1) try writer.writeAll("\n");
    }
    switch (indent) {
        .space => |space| {
            var i: u64 = 0;
            while (i < space) : (i += 1) try writer.writeAll(" ");
        },
        .tab => |tab| {
            var i: u64 = 0;
            while (i < tab) : (i += 1) try writer.writeAll("\t");
        },
    }
}

pub fn toSource(allocator: Allocator, intern: Intern, tokens: Tokens) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    var pos = Pos{ .line = 1, .column = 1 };
    for (tokens.kind.items) |kind, i| {
        const span = tokens.span.items[i];
        try spaceToSource(writer, span, &pos);
        switch (kind) {
            .symbol, .int, .float => |interned| {
                const string = interner.lookup(intern, interned);
                try writer.writeAll(string);
            },
            .bool => |b| try writer.writeAll(if (b) "true" else "false"),
            .indent => |indent| try indentToSource(writer, span, indent),
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
        }
    }
    return list.toOwnedSlice();
}
