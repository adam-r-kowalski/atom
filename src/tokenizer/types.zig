const Interned = @import("../interner.zig").Interned;

pub const Pos = struct { line: u64, column: u64 };
pub const Span = struct { begin: Pos, end: Pos };
const Symbol = struct { value: Interned, span: Span };
const Int = struct { value: Interned, span: Span };
const Float = struct { value: Interned, span: Span };
const String = struct { value: Interned, span: Span };
const Bool = struct { value: bool, span: Span };
const Equal = struct { span: Span };
const EqualEqual = struct { span: Span };
const Dot = struct { span: Span };
const Colon = struct { span: Span };
const Plus = struct { span: Span };
const Minus = struct { span: Span };
const Times = struct { span: Span };
const Caret = struct { span: Span };
const Greater = struct { span: Span };
const Less = struct { span: Span };
const Percent = struct { span: Span };
const LeftParen = struct { span: Span };
const RightParen = struct { span: Span };
const LeftBrace = struct { span: Span };
const RightBrace = struct { span: Span };
const If = struct { span: Span };
const Else = struct { span: Span };
const Or = struct { span: Span };
const Comma = struct { span: Span };
const Fn = struct { span: Span };
const NewLine = struct { span: Span };

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
};
