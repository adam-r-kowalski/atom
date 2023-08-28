const std = @import("std");
const type_checker = @import("../type_checker.zig");
const types = @import("types.zig");

pub fn int(i: type_checker.types.Int) !types.Expression {
    switch (i.type) {
        .u8 => return .{ .literal = .{ .i32 = try std.fmt.parseInt(u8, i.value.string(), 10) } },
        .u32 => return .{ .literal = .{ .u32 = try std.fmt.parseInt(u32, i.value.string(), 10) } },
        .u64 => return .{ .literal = .{ .u64 = try std.fmt.parseInt(u64, i.value.string(), 10) } },
        .i32 => return .{ .literal = .{ .i32 = try std.fmt.parseInt(i32, i.value.string(), 10) } },
        .i64 => return .{ .literal = .{ .i64 = try std.fmt.parseInt(i64, i.value.string(), 10) } },
        .f32 => return .{ .literal = .{ .f32 = i.value } },
        .f64 => return .{ .literal = .{ .f64 = i.value } },
        else => |k| std.debug.panic("\nInt type {} not yet supported", .{k}),
    }
}

pub fn float(f: type_checker.types.Float) !types.Expression {
    switch (f.type) {
        .f32 => return .{ .literal = .{ .f32 = f.value } },
        .f64 => return .{ .literal = .{ .f64 = f.value } },
        else => |k| std.debug.panic("\nFloat type {} not yet supported", .{k}),
    }
}

pub fn boolean(b: type_checker.types.Bool) !types.Expression {
    return types.Expression{ .literal = types.Literal{ .bool = b.value } };
}

pub fn symbol(s: type_checker.types.Symbol) types.Expression {
    if (s.binding.global) return types.Expression{ .global_get = .{ .name = s.value } };
    return .{ .local_get = .{ .name = s.value } };
}
