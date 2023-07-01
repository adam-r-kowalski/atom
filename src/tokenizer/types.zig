const Interned = @import("../interner.zig").Interned;

pub const Pos = struct { line: u64, column: u64 };
pub const Span = struct { begin: Pos, end: Pos };

pub const Comment = struct { value: Interned, span: Span };
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
pub const PlusEqual = struct { span: Span };
pub const Minus = struct { span: Span };
pub const Times = struct { span: Span };
pub const TimesEqual = struct { span: Span };
pub const Slash = struct { span: Span };
pub const Caret = struct { span: Span };
pub const Greater = struct { span: Span };
pub const Less = struct { span: Span };
pub const Percent = struct { span: Span };
pub const LeftParen = struct { span: Span };
pub const RightParen = struct { span: Span };
pub const LeftBrace = struct { span: Span };
pub const RightBrace = struct { span: Span };
pub const LeftBracket = struct { span: Span };
pub const RightBracket = struct { span: Span };
pub const If = struct { span: Span };
pub const Else = struct { span: Span };
pub const Or = struct { span: Span };
pub const Comma = struct { span: Span };
pub const Fn = struct { span: Span };
pub const Enum = struct { span: Span };
pub const Mut = struct { span: Span };
pub const Undefined = struct { span: Span };
pub const NewLine = struct { span: Span };

pub const Token = union(enum) {
    comment: Comment,
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
    plus_equal: PlusEqual,
    minus: Minus,
    times: Times,
    times_equal: TimesEqual,
    slash: Slash,
    caret: Caret,
    greater: Greater,
    less: Less,
    percent: Percent,
    left_paren: LeftParen,
    right_paren: RightParen,
    left_brace: LeftBrace,
    right_brace: RightBrace,
    left_bracket: LeftBracket,
    right_bracket: RightBracket,
    if_: If,
    else_: Else,
    or_: Or,
    comma: Comma,
    fn_: Fn,
    enum_: Enum,
    mut: Mut,
    undefined: Undefined,
    new_line: NewLine,
};
