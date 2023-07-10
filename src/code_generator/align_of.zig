const std = @import("std");
const Enumeration = @import("../type_checker/monotype.zig").Enumeration;
const Structure = @import("../type_checker/monotype.zig").Structure;
const StructureLiteral = @import("../type_checker/monotype.zig").StructureLiteral;
const MonoType = @import("../type_checker/monotype.zig").MonoType;

pub fn enumeration(e: Enumeration) u32 {
    return switch (e.variants.len) {
        0...31 => 4,
        32...63 => 8,
        else => std.debug.panic("\nToo many variants in enum\n", .{}),
    };
}

pub fn structure(s: Structure) u32 {
    var alignment: u32 = 0;
    for (s.order) |o| {
        const field = s.fields.get(o).?;
        alignment = @max(alignment, monotype(field));
    }
    return alignment;
}

pub fn structureLiteral(s: StructureLiteral) u32 {
    return monotype(s.structure.*);
}

pub fn monotype(m: MonoType) u32 {
    return switch (m) {
        .void => 0,
        .u8 => 1,
        .i32 => 4,
        .i64 => 8,
        .f32 => 4,
        .f64 => 8,
        .bool => 1,
        .enumeration => |e| enumeration(e),
        .structure => |s| structure(s),
        .structure_literal => |s| structureLiteral(s),
        .array => 4,
        else => |k| std.debug.panic("\nSize of {} is unknown\n", .{k}),
    };
}
