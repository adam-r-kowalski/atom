const std = @import("std");
const type_checker = @import("../type_checker.zig");
const types = @import("types.zig");
const size_of = @import("size_of.zig");

pub fn mapType(monotype: type_checker.types.MonoType) types.Type {
    switch (monotype) {
        .u8 => return .i32,
        .u32 => return .i32,
        .u64 => return .i64,
        .i32 => return .i32,
        .i64 => return .i64,
        .f32 => return .f32,
        .f64 => return .f64,
        .bool => return .i32,
        .void => return .void,
        .array => return .i32,
        .enumeration => |e| {
            switch (size_of.enumeration(e)) {
                0...4 => return .i32,
                5...8 => return .i64,
                else => std.debug.panic("\nEnumeration with {} variants not yet supported", .{e.variants.len}),
            }
        },
        .structure => return .i32,
        else => std.debug.panic("\nMonotype {} not yet supported", .{monotype}),
    }
}
