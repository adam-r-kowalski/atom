const interner = @import("interner.zig");
const Intern = interner.Intern;
const Interned = interner.Interned;

pub const Builtins = struct {
    fn_: Interned,
    enum_: Interned,
    struct_: Interned,
    for_: Interned,
    block: Interned,
    u8: Interned,
    u32: Interned,
    u64: Interned,
    i32: Interned,
    i64: Interned,
    f32: Interned,
    f64: Interned,
    bool: Interned,
    void: Interned,
    str: Interned,
    if_: Interned,
    else_: Interned,
    true_: Interned,
    false_: Interned,
    or_: Interned,
    import: Interned,
    export_: Interned,
    convert: Interned,
    sqrt: Interned,
    empty: Interned,
    core_arena: Interned,
    mut: Interned,
    undefined: Interned,
    one: Interned,
    underscore: Interned,
    host: Interned,

    pub fn init(intern: *Intern) !Builtins {
        return Builtins{
            .fn_ = try intern.store("fn"),
            .enum_ = try intern.store("enum"),
            .struct_ = try intern.store("struct"),
            .for_ = try intern.store("for"),
            .block = try intern.store("block"),
            .u8 = try intern.store("u8"),
            .u32 = try intern.store("u32"),
            .u64 = try intern.store("u64"),
            .i32 = try intern.store("i32"),
            .i64 = try intern.store("i64"),
            .f32 = try intern.store("f32"),
            .f64 = try intern.store("f64"),
            .bool = try intern.store("bool"),
            .void = try intern.store("void"),
            .str = try intern.store("str"),
            .if_ = try intern.store("if"),
            .else_ = try intern.store("else"),
            .true_ = try intern.store("true"),
            .false_ = try intern.store("false"),
            .or_ = try intern.store("or"),
            .import = try intern.store("@import"),
            .export_ = try intern.store("@export"),
            .convert = try intern.store("convert"),
            .sqrt = try intern.store("sqrt"),
            .empty = try intern.store("empty"),
            .core_arena = try intern.store("core/arena"),
            .mut = try intern.store("mut"),
            .undefined = try intern.store("undefined"),
            .one = try intern.store("1"),
            .underscore = try intern.store("_"),
            .host = try intern.store("host"),
        };
    }
};
