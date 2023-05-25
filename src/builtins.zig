const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;

pub const Builtins = struct {
    fn_: Interned,
    i32: Interned,
    f32: Interned,
    str: Interned,
    bool: Interned,
    void: Interned,
    if_: Interned,
    else_: Interned,
    true_: Interned,
    false_: Interned,
    or_: Interned,
    one: Interned,
    zero: Interned,
    foreign_import: Interned,

    pub fn init(intern: *Intern) !Builtins {
        return Builtins{
            .fn_ = try interner.store(intern, "fn"),
            .i32 = try interner.store(intern, "i32"),
            .f32 = try interner.store(intern, "f32"),
            .str = try interner.store(intern, "str"),
            .bool = try interner.store(intern, "bool"),
            .void = try interner.store(intern, "void"),
            .if_ = try interner.store(intern, "if"),
            .else_ = try interner.store(intern, "else"),
            .true_ = try interner.store(intern, "true"),
            .false_ = try interner.store(intern, "false"),
            .or_ = try interner.store(intern, "or"),
            .one = try interner.store(intern, "1"),
            .zero = try interner.store(intern, "0"),
            .foreign_import = try interner.store(intern, "foreign_import"),
        };
    }
};
