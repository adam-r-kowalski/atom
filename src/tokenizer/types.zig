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
    left_brace,
    right_brace,
    if_,
    comma,
    fn_,
    import,
    export_,
};

pub const Token = struct {
    kind: Kind,
    span: Span,
};
