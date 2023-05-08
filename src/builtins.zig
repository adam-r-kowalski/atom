const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;

pub const Builtins = struct {
    if_: Interned,
    then: Interned,
    else_: Interned,
    i32: Interned,
    bool: Interned,
    true_: Interned,
    false_: Interned,

    pub fn init(intern: *Intern) !Builtins {
        return Builtins{
            .if_ = try interner.store(intern, "if"),
            .then = try interner.store(intern, "then"),
            .else_ = try interner.store(intern, "else"),
            .i32 = try interner.store(intern, "i32"),
            .bool = try interner.store(intern, "bool"),
            .true_ = try interner.store(intern, "true"),
            .false_ = try interner.store(intern, "false"),
        };
    }
};
