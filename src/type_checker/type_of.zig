const types = @import("types.zig");

pub fn expression(e: types.Expression) types.MonoType {
    return switch (e) {
        .int => |i| i.type,
        .float => |f| f.type,
        .symbol => |s| s.type,
        .bool => |b| b.type,
        .string => |s| s.type,
        .define => |d| d.type,
        .drop => |d| d.type,
        .plus_equal => |p| p.type,
        .times_equal => |t| t.type,
        .function => |f| f.type,
        .prototype => |p| p.type,
        .binary_op => |b| b.type,
        .dot => |d| d.type,
        .group => |g| g.type,
        .block => |b| b.type,
        .branch => |b| b.type,
        .call => |c| c.type,
        .decorator => |d| d.type,
        .intrinsic => |i| i.type,
        .foreign_import => |f| f.type,
        .foreign_export => |f| f.type,
        .convert => |c| c.type,
        .undefined => |u| u.type,
        .array => |a| a.type,
        .index => |i| i.type,
        .template_literal => |t| t.type,
    };
}
