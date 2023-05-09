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
    import,
    export_,
};

const Pos = struct {
    line: u64,
    column: u64,
};

pub const Span = struct {
    begin: Pos,
    end: Pos,
};

pub const Token = struct {
    kind: Kind,
    span: Span,
};

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
            '-' => return choice(cursor, .minus, &.{.{ '>', .arrow }}),
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

fn exact(cursor: *Cursor, kind: Kind) Token {
    const begin = cursor.pos;
    _ = advance(cursor, 1);
    return Token{ .kind = kind, .span = .{ .begin = begin, .end = cursor.pos } };
}

const Choice = Tuple(&.{ u8, Kind });

fn choice(cursor: *Cursor, kind: Kind, choices: []const Choice) Token {
    const begin = cursor.pos;
    if (cursor.source.len > 1) {
        const t = cursor.source[1];
        for (choices) |c| {
            if (t == c[0]) {
                _ = advance(cursor, 2);
                return Token{ .kind = c[1], .span = .{ .begin = begin, .end = cursor.pos } };
            }
        }
    }
    _ = advance(cursor, 1);
    return Token{ .kind = kind, .span = .{ .begin = begin, .end = cursor.pos } };
}

fn symbol(intern: *Intern, builtins: Builtins, cursor: *Cursor) !Token {
    const begin = cursor.pos;
    var i: u64 = 0;
    while (i < cursor.source.len and !reserved(cursor.source[i])) : (i += 1) {}
    const string = advance(cursor, i);
    const end = cursor.pos;
    const span = Span{ .begin = begin, .end = end };
    const interned = try interner.store(intern, string);
    if (interned == builtins.import) return Token{ .kind = .import, .span = span };
    if (interned == builtins.export_) return Token{ .kind = .export_, .span = span };
    if (interned == builtins.if_) return Token{ .kind = .if_, .span = span };
    if (interned == builtins.then) return Token{ .kind = .then, .span = span };
    if (interned == builtins.else_) return Token{ .kind = .else_, .span = span };
    if (interned == builtins.true_) return Token{ .kind = .{ .bool = true }, .span = span };
    if (interned == builtins.false_) return Token{ .kind = .{ .bool = false }, .span = span };
    return Token{ .kind = .{ .symbol = interned }, .span = span };
}

fn newLine(cursor: *Cursor) ?Token {
    var begin = cursor.pos;
    cursor.pos.column = 1;
    var i: u64 = 0;
    while (cursor.source.len > i and cursor.source[i] == '\n') : (i += 1) {}
    cursor.pos.line += i;
    cursor.source = cursor.source[i..];
    if (cursor.source.len == 0) return null;
    i = 0;
    if (cursor.source[0] == ' ') {
        while (cursor.source.len > i and cursor.source[i] == ' ') : (i += 1) {}
        cursor.pos.column += i;
        cursor.source = cursor.source[i..];
        if (cursor.source.len == 0) return null;
        return Token{
            .kind = .{ .indent = .{ .space = i } },
            .span = .{ .begin = begin, .end = cursor.pos },
        };
    }
    if (cursor.source[0] == '\t') {
        while (cursor.source.len > i and cursor.source[i] == '\t') : (i += 1) {}
        cursor.pos.column += i;
        cursor.source = cursor.source[i..];
        if (cursor.source.len == 0) return null;
        return Token{
            .kind = .{ .indent = .{ .tab = i } },
            .span = .{ .begin = begin, .end = cursor.pos },
        };
    }
    return Token{
        .kind = .{ .indent = .{ .space = 0 } },
        .span = .{ .begin = begin, .end = cursor.pos },
    };
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
        ',' => exact(cursor, .comma),
        '\n' => newLine(cursor),
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

pub fn toString(allocator: Allocator, intern: Intern, tokens: []const Token) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    for (tokens) |token, i| {
        if (i != 0) try writer.writeAll("\n");
        switch (token.kind) {
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
            .import => try writer.writeAll("import"),
            .export_ => try writer.writeAll("export"),
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

pub fn toSource(allocator: Allocator, intern: Intern, tokens: []const Token) ![]const u8 {
    var list = List(u8).init(allocator);
    const writer = list.writer();
    var pos = Pos{ .line = 1, .column = 1 };
    for (tokens) |token| {
        try spaceToSource(writer, token.span, &pos);
        switch (token.kind) {
            .symbol, .int, .float => |interned| {
                const string = interner.lookup(intern, interned);
                try writer.writeAll(string);
            },
            .bool => |b| try writer.writeAll(if (b) "true" else "false"),
            .indent => |indent| try indentToSource(writer, token.span, indent),
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
