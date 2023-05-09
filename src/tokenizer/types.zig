const Interned = @import("../interner.zig").Interned;

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

pub const Pos = struct {
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
