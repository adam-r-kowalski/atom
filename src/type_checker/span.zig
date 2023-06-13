const types = @import("types.zig");

pub fn expression(e: types.Expression) types.Span {
    return switch (e) {
        .int => |i| i.span,
        .float => |f| f.span,
        .symbol => |s| s.span,
        .bool => |b| b.span,
        .string => |s| s.span,
        .define => |d| d.span,
        .add_assign => |a| a.span,
        .function => |f| f.span,
        .binary_op => |b| b.span,
        .group => |g| g.span,
        .block => |b| b.span,
        .branch => |b| b.span,
        .call => |c| c.span,
        .intrinsic => |i| i.span,
        .foreign_import => |f| f.span,
        .foreign_export => |f| f.span,
        .convert => |c| c.span,
        .undefined => |u| u.span,
    };
}
