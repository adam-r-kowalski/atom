const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;

pub const Builtins = struct {
    fn_: Interned,
    i32: Interned,
    f32: Interned,
    bool: Interned,
    if_: Interned,
    else_: Interned,
    true_: Interned,
    false_: Interned,

    pub fn init(intern: *Intern) !Builtins {
        return Builtins{
            .fn_ = try interner.store(intern, "fn"),
            .i32 = try interner.store(intern, "i32"),
            .f32 = try interner.store(intern, "f32"),
            .bool = try interner.store(intern, "bool"),
            .if_ = try interner.store(intern, "if"),
            .else_ = try interner.store(intern, "else"),
            .true_ = try interner.store(intern, "true"),
            .false_ = try interner.store(intern, "false"),
        };
    }
};
