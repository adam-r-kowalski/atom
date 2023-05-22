const Interned = @import("../interner.zig").Interned;

pub const Pos = struct {
    line: u64,
    column: u64,
};

pub const Span = struct {
    begin: Pos,
    end: Pos,
};

pub const Kind = union(enum) {
    symbol: Interned,
    int: Interned,
    float: Interned,
    string: Interned,
    bool: bool,
    equal,
    equal_equal,
    dot,
    colon,
    plus,
    minus,
    times,
    caret,
    greater,
    less,
    percent,
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    if_,
    else_,
    or_,
    comma,
    fn_,
    new_line,
};

pub const Token = struct {
    kind: Kind,
    span: Span,
};
