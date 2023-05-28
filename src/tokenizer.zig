const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("interner.zig");
const Interned = interner.Interned;
const Intern = interner.Intern;
const Builtins = @import("builtins.zig").Builtins;

pub const Pos = struct { line: u64, column: u64 };
pub const Span = struct { begin: Pos, end: Pos };
pub const Symbol = struct { value: Interned, span: Span };
pub const Int = struct { value: Interned, span: Span };
pub const Float = struct { value: Interned, span: Span };
pub const String = struct { value: Interned, span: Span };
pub const Bool = struct { value: bool, span: Span };
pub const Equal = struct { span: Span };
pub const EqualEqual = struct { span: Span };
pub const Dot = struct { span: Span };
pub const Colon = struct { span: Span };
pub const Plus = struct { span: Span };
pub const Minus = struct { span: Span };
pub const Times = struct { span: Span };
pub const Slash = struct { span: Span };
pub const Caret = struct { span: Span };
pub const Greater = struct { span: Span };
pub const Less = struct { span: Span };
pub const Percent = struct { span: Span };
pub const LeftParen = struct { span: Span };
pub const RightParen = struct { span: Span };
pub const LeftBrace = struct { span: Span };
pub const RightBrace = struct { span: Span };
pub const If = struct { span: Span };
pub const Else = struct { span: Span };
pub const Or = struct { span: Span };
pub const Comma = struct { span: Span };
pub const Fn = struct { span: Span };
pub const NewLine = struct { span: Span };

pub const Token = union(enum) {
    symbol: Symbol,
    int: Int,
    float: Float,
    string: String,
    bool: Bool,
    equal: Equal,
    equal_equal: EqualEqual,
    dot: Dot,
    colon: Colon,
    plus: Plus,
    minus: Minus,
    times: Times,
    slash: Slash,
    caret: Caret,
    greater: Greater,
    less: Less,
    percent: Percent,
    left_paren: LeftParen,
    right_paren: RightParen,
    left_brace: LeftBrace,
    right_brace: RightBrace,
    if_: If,
    else_: Else,
    or_: Or,
    comma: Comma,
    fn_: Fn,
    new_line: NewLine,

    pub fn span(self: Token) Span {
        return switch (self) {
            .symbol => |t| t.span,
            .int => |t| t.span,
            .float => |t| t.span,
            .string => |t| t.span,
            .bool => |t| t.span,
            .equal => |t| t.span,
            .equal_equal => |t| t.span,
            .dot => |t| t.span,
            .colon => |t| t.span,
            .plus => |t| t.span,
            .minus => |t| t.span,
            .times => |t| t.span,
            .slash => |t| t.span,
            .caret => |t| t.span,
            .greater => |t| t.span,
            .less => |t| t.span,
            .percent => |t| t.span,
            .left_paren => |t| t.span,
            .right_paren => |t| t.span,
            .left_brace => |t| t.span,
            .right_brace => |t| t.span,
            .if_ => |t| t.span,
            .else_ => |t| t.span,
            .or_ => |t| t.span,
            .comma => |t| t.span,
            .fn_ => |t| t.span,
            .new_line => |t| t.span,
        };
    }
};

pub const Tokens = struct {
    tokens: []Token,
    index: usize,
    intern: *Intern,

    pub fn peek(self: Tokens) ?Token {
        if (self.index >= self.tokens.len) return null;
        return self.tokens[self.index];
    }

    pub fn next(self: *Tokens) ?Token {
        if (self.index >= self.tokens.len) return null;
        const index = self.index;
        self.index += 1;
        return self.tokens[index];
    }

    pub fn advance(self: *Tokens) void {
        self.index += 1;
    }

    pub fn format(
        self: Tokens,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        for (self.tokens, 0..) |token, i| {
            if (i != 0) try writer.writeAll("\n");
            switch (token) {
                .symbol => |s| try writer.print("symbol {s}", .{self.intern.lookup(s.value)}),
                .int => |s| try writer.print("int {s}", .{self.intern.lookup(s.value)}),
                .float => |s| try writer.print("float {s}", .{self.intern.lookup(s.value)}),
                .string => |s| try writer.print("string {s}", .{self.intern.lookup(s.value)}),
                .bool => |b| try writer.print("bool {}", .{b.value}),
                .equal => try writer.writeAll("equal"),
                .equal_equal => try writer.writeAll("equal equal"),
                .dot => try writer.writeAll("dot"),
                .colon => try writer.writeAll("colon"),
                .plus => try writer.writeAll("plus"),
                .minus => try writer.writeAll("minus"),
                .times => try writer.writeAll("times"),
                .slash => try writer.writeAll("slash"),
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
    }

    pub fn toSource(self: Tokens, allocator: Allocator) ![]const u8 {
        var list = List(u8).init(allocator);
        const writer = list.writer();
        var pos = Pos{ .line = 1, .column = 1 };
        for (self.tokens) |token| {
            const current_span = token.span();
            const delta_lines = current_span.end.line - pos.line;
            for (0..delta_lines) |_| try writer.writeAll("\n");
            const delta_columns = current_span.begin.column - pos.column;
            for (0..delta_columns) |_| try writer.writeAll(" ");
            pos = current_span.end;
            switch (token) {
                .symbol => |s| try writer.writeAll(self.intern.lookup(s.value)),
                .int => |i| try writer.writeAll(self.intern.lookup(i.value)),
                .float => |f| try writer.writeAll(self.intern.lookup(f.value)),
                .string => |s| try writer.writeAll(self.intern.lookup(s.value)),
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
};

const Cursor = struct {
    source: []const u8,
    pos: Pos,
};

fn trim(cursor: *Cursor) void {
    var i: u64 = 0;
    while (i < cursor.source.len) {
        switch (cursor.source[i]) {
            ' ', '\t' => i += 1,
            else => break,
        }
    }
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
    if (i >= 2 and cursor.source[i - 1] == '.') {
        i -= 1;
        decimals -= 1;
    }
    const contents = cursor.source[0..i];
    if (contents.len == 1) {
        switch (contents[0]) {
            '-' => return exact(cursor, .minus),
            '.' => {
                _ = advance(cursor, i);
                return Token{ .dot = .{ .span = .{ .begin = begin, .end = cursor.pos } } };
            },
            else => {},
        }
    }
    _ = advance(cursor, i);
    const span = Span{ .begin = begin, .end = cursor.pos };
    const interned = try intern.store(contents);
    if (decimals == 0) return Token{ .int = .{ .value = interned, .span = span } };
    return Token{ .float = .{ .value = interned, .span = span } };
}

fn string(intern: *Intern, cursor: *Cursor) !Token {
    const begin = cursor.pos;
    var i: u64 = 1;
    while (i < cursor.source.len) : (i += 1) {
        if (cursor.source[i] == '"') {
            i += 1;
            break;
        }
    }
    const contents = cursor.source[0..i];
    _ = advance(cursor, i);
    const span = Span{ .begin = begin, .end = cursor.pos };
    const interned = try intern.store(contents);
    return Token{ .string = .{ .value = interned, .span = span } };
}

const Tag = std.meta.Tag(Token);

fn exact(cursor: *Cursor, comptime tag: Tag) Token {
    const begin = cursor.pos;
    _ = advance(cursor, 1);
    const span = Span{ .begin = begin, .end = cursor.pos };
    return @unionInit(Token, @tagName(tag), .{ .span = span });
}

fn symbol(intern: *Intern, builtins: Builtins, cursor: *Cursor) !Token {
    const begin = cursor.pos;
    var i: u64 = 0;
    while (i < cursor.source.len and !reserved(cursor.source[i])) : (i += 1) {}
    const contents = advance(cursor, i);
    const end = cursor.pos;
    const span = Span{ .begin = begin, .end = end };
    const interned = try intern.store(contents);
    if (interned == builtins.fn_) return Token{ .fn_ = .{ .span = span } };
    if (interned == builtins.if_) return Token{ .if_ = .{ .span = span } };
    if (interned == builtins.else_) return Token{ .else_ = .{ .span = span } };
    if (interned == builtins.true_) return Token{ .bool = .{ .value = true, .span = span } };
    if (interned == builtins.false_) return Token{ .bool = .{ .value = false, .span = span } };
    if (interned == builtins.or_) return Token{ .or_ = .{ .span = span } };
    return Token{ .symbol = .{ .value = interned, .span = span } };
}

fn newLine(cursor: *Cursor) Token {
    const begin = cursor.pos;
    var i: u64 = 0;
    while (i < cursor.source.len and cursor.source[i] == '\n') : (i += 1) {}
    cursor.pos.line += i;
    cursor.pos.column = 1;
    cursor.source = cursor.source[i..];
    return Token{ .new_line = .{ .span = .{ .begin = begin, .end = cursor.pos } } };
}

fn either(cursor: *Cursor, comptime tag: Tag, comptime char: u8, comptime other: Tag) Token {
    const begin = cursor.pos;
    if (cursor.source.len > 1 and cursor.source[1] == char) {
        _ = advance(cursor, 2);
        const span = Span{ .begin = begin, .end = cursor.pos };
        return @unionInit(Token, @tagName(other), .{ .span = span });
    }
    _ = advance(cursor, 1);
    const span = Span{ .begin = begin, .end = cursor.pos };
    return @unionInit(Token, @tagName(tag), .{ .span = span });
}

fn nextToken(cursor: *Cursor, intern: *Intern, builtins: Builtins) !?Token {
    trim(cursor);
    if (cursor.source.len == 0) return null;
    return switch (cursor.source[0]) {
        '0'...'9', '-', '.' => try number(intern, cursor),
        '"' => try string(intern, cursor),
        '=' => either(cursor, .equal, '=', .equal_equal),
        ':' => exact(cursor, .colon),
        '+' => exact(cursor, .plus),
        '*' => exact(cursor, .times),
        '/' => exact(cursor, .slash),
        '^' => exact(cursor, .caret),
        '>' => exact(cursor, .greater),
        '<' => exact(cursor, .less),
        '%' => exact(cursor, .percent),
        '(' => exact(cursor, .left_paren),
        ')' => exact(cursor, .right_paren),
        '{' => exact(cursor, .left_brace),
        '}' => exact(cursor, .right_brace),
        ',' => exact(cursor, .comma),
        '\n' => newLine(cursor),
        else => try symbol(intern, builtins, cursor),
    };
}

pub fn tokenize(allocator: Allocator, intern: *Intern, builtins: Builtins, source: []const u8) !Tokens {
    var cursor = Cursor{
        .source = source,
        .pos = .{ .line = 1, .column = 1 },
    };
    var tokens = List(Token).init(allocator);
    while (try nextToken(&cursor, intern, builtins)) |token|
        try tokens.append(token);
    return Tokens{
        .tokens = try tokens.toOwnedSlice(),
        .index = 0,
        .intern = intern,
    };
}
