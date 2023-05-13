const Interned = @import("../interner.zig").Interned;

pub const Pos = struct {
    line: u64,
    column: u64,
};

pub const Span = struct {
    begin: Pos,
    end: Pos,
};

pub const Indent = struct {
    kind: enum {
        space,
        tab,
    },
    count: u64,
    span: Span,
};

pub const Symbol = struct {
    value: Interned,
    span: Span,
};

pub const Int = struct {
    value: Interned,
    span: Span,
};

pub const Float = struct {
    value: Interned,
    span: Span,
};

pub const Bool = struct {
    value: bool,
    span: Span,
};

pub const Equal = struct { span: Span };

pub const Dot = struct { span: Span };

pub const Colon = struct { span: Span };

pub const Plus = struct { span: Span };

pub const Minus = struct { span: Span };

pub const Times = struct { span: Span };

pub const Caret = struct { span: Span };

pub const Greater = struct { span: Span };

pub const Less = struct { span: Span };

pub const LeftParen = struct { span: Span };

pub const RightParen = struct { span: Span };

pub const If = struct { span: Span };

pub const Then = struct { span: Span };

pub const Else = struct { span: Span };

pub const Comma = struct { span: Span };

pub const Arrow = struct { span: Span };

pub const Import = struct { span: Span };

pub const Export = struct { span: Span };

pub const Token = union(enum) {
    symbol: Symbol,
    int: Int,
    float: Float,
    indent: Indent,
    bool: Bool,
    equal: Equal,
    dot: Dot,
    colon: Colon,
    plus: Plus,
    minus: Minus,
    times: Times,
    caret: Caret,
    greater: Greater,
    less: Less,
    left_paren: LeftParen,
    right_paren: RightParen,
    if_: If,
    then: Then,
    else_: Else,
    comma: Comma,
    arrow: Arrow,
    import: Import,
    export_: Export,
};
