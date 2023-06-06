const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;

pub const Builtins = struct {
    fn_: Interned,
    i32: Interned,
    i64: Interned,
    f32: Interned,
    f64: Interned,
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
    foreign_export: Interned,
    convert: Interned,
    sqrt: Interned,
    arena: Interned,

    pub fn init(intern: *Intern) !Builtins {
        return Builtins{
            .fn_ = try intern.store("fn"),
            .i32 = try intern.store("i32"),
            .i64 = try intern.store("i64"),
            .f32 = try intern.store("f32"),
            .f64 = try intern.store("f64"),
            .bool = try intern.store("bool"),
            .void = try intern.store("void"),
            .if_ = try intern.store("if"),
            .else_ = try intern.store("else"),
            .true_ = try intern.store("true"),
            .false_ = try intern.store("false"),
            .or_ = try intern.store("or"),
            .one = try intern.store("1"),
            .zero = try intern.store("0"),
            .foreign_import = try intern.store("foreign_import"),
            .foreign_export = try intern.store("foreign_export"),
            .convert = try intern.store("convert"),
            .sqrt = try intern.store("sqrt"),
            .arena = try intern.store("arena"),
        };
    }
};
