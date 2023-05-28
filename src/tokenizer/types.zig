const std = @import("std");
const Interned = @import("../interner.zig").Interned;

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
        _ = self;
        _ = fmt;
        _ = options;

        try writer.writeAll(")");
    }
};
