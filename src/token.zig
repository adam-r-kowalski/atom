const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("interner.zig");
const Interned = interner.Interned;
const Intern = interner.Intern;
const CompileErrors = @import("compile_errors.zig").CompileErrors;
const Span = @import("span.zig").Span;
const Pos = @import("span.zig").Pos;

pub const Symbol = struct {
    value: Interned,
    span: Span,

    pub fn format(self: Symbol, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(symbol {})", .{self.value});
    }
};

pub const Int = struct {
    value: Interned,
    span: Span,

    pub fn format(self: Int, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(int {})", .{self.value});
    }
};

pub const Float = struct {
    value: Interned,
    span: Span,

    pub fn format(self: Float, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(float {})", .{self.value});
    }
};

pub const String = struct {
    value: Interned,
    span: Span,

    pub fn format(self: String, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(string {})", .{self.value});
    }
};

pub const Bool = struct {
    value: bool,
    span: Span,

    pub fn format(self: Bool, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("(bool {})", .{self.value});
    }
};

pub const Equal = struct {
    span: Span,

    pub fn format(self: Equal, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(operator =)");
    }
};

pub const EqualEqual = struct {
    span: Span,

    pub fn format(self: EqualEqual, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(operator ==)");
    }
};

pub const Dot = struct {
    span: Span,

    pub fn format(self: Dot, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(operator .)");
    }
};

pub const Colon = struct {
    span: Span,

    pub fn format(self: Colon, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(operator :)");
    }
};

pub const Plus = struct {
    span: Span,

    pub fn format(self: Plus, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(operator +)");
    }
};

pub const Minus = struct {
    span: Span,

    pub fn format(self: Minus, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(operator -)");
    }
};

pub const Times = struct {
    span: Span,

    pub fn format(self: Times, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(operator *)");
    }
};

pub const Slash = struct {
    span: Span,

    pub fn format(self: Slash, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(operator /)");
    }
};

pub const Caret = struct {
    span: Span,

    pub fn format(self: Caret, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(operator ^)");
    }
};

pub const Greater = struct {
    span: Span,

    pub fn format(self: Greater, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(operator >)");
    }
};

pub const Less = struct {
    span: Span,

    pub fn format(self: Less, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(operator <)");
    }
};

pub const Percent = struct {
    span: Span,

    pub fn format(self: Percent, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(operator %)");
    }
};

pub const LeftParen = struct {
    span: Span,

    pub fn format(self: LeftParen, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(delimiter '(')");
    }
};

pub const RightParen = struct {
    span: Span,

    pub fn format(self: RightParen, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(delimiter ')')");
    }
};

pub const LeftBrace = struct {
    span: Span,

    pub fn format(self: LeftBrace, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(delimiter '{')");
    }
};

pub const RightBrace = struct {
    span: Span,

    pub fn format(self: RightBrace, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(delimiter '}')");
    }
};

pub const If = struct {
    span: Span,

    pub fn format(self: If, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(keyword if)");
    }
};

pub const Else = struct {
    span: Span,

    pub fn format(self: Else, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(keyword else)");
    }
};

pub const Or = struct {
    span: Span,

    pub fn format(self: Or, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(keyword or)");
    }
};

pub const Comma = struct {
    span: Span,

    pub fn format(self: Comma, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(delimiter ',')");
    }
};

pub const Fn = struct {
    span: Span,

    pub fn format(self: Fn, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(keyword fn)");
    }
};

pub const NewLine = struct {
    span: Span,

    pub fn format(self: NewLine, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        _ = options;
        _ = fmt;
        try writer.writeAll("(new_line)");
    }
};

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

    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .symbol => |s| try writer.print("{}", .{s}),
            .int => |i| try writer.print("{}", .{i}),
            .float => |f| try writer.print("{}", .{f}),
            .string => |s| try writer.print("{}", .{s}),
            .bool => |b| try writer.print("{}", .{b}),
            .equal => |e| try writer.print("{}", .{e}),
            .equal_equal => |e| try writer.print("{}", .{e}),
            .dot => |d| try writer.print("{}", .{d}),
            .colon => |c| try writer.print("{}", .{c}),
            .plus => |p| try writer.print("{}", .{p}),
            .minus => |m| try writer.print("{}", .{m}),
            .times => |t| try writer.print("{}", .{t}),
            .slash => |s| try writer.print("{}", .{s}),
            .percent => |p| try writer.print("{}", .{p}),
            .caret => |c| try writer.print("{}", .{c}),
            .greater => |g| try writer.print("{}", .{g}),
            .less => |l| try writer.print("{}", .{l}),
            .left_paren => |l| try writer.print("{}", .{l}),
            .right_paren => |r| try writer.print("{}", .{r}),
            .left_brace => |l| try writer.print("{}", .{l}),
            .right_brace => |r| try writer.print("{}", .{r}),
            .if_ => |i| try writer.print("{}", .{i}),
            .else_ => |e| try writer.print("{}", .{e}),
            .or_ => |o| try writer.print("{}", .{o}),
            .comma => |c| try writer.print("{}", .{c}),
            .fn_ => |f| try writer.print("{}", .{f}),
            .new_line => |n| try writer.print("{}", .{n}),
        }
    }
};

pub const Tokens = struct {
    tokens: []Token,
    index: usize,
    intern: *Intern,
    compile_errors: *CompileErrors,

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

    pub fn format(self: Tokens, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        for (self.tokens, 0..) |token, i| {
            if (i != 0) try writer.writeAll("\n");
            try writer.print("{}", .{token});
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
