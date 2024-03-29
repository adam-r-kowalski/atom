const types = @import("types.zig");

pub fn expression(e: types.Expression) types.Span {
    return switch (e) {
        .int => |i| i.span,
        .float => |f| f.span,
        .symbol => |s| s.span,
        .string => |s| s.span,
        .bool => |b| b.span,
        .define => |d| d.span,
        .drop => |d| d.span,
        .plus_equal => |a| a.span,
        .times_equal => |a| a.span,
        .function => |f| f.span,
        .prototype => |p| p.span,
        .enumeration => |en| en.span,
        .structure => |s| s.span,
        .binary_op => |b| b.span,
        .dot => |d| d.span,
        .group => |g| g.span,
        .block => |b| b.span,
        .array => |a| a.span,
        .array_type => |a| a.span,
        .branch => |b| b.span,
        .call => |c| c.span,
        .decorator => |d| d.span,
        .index => |i| i.span,
        .template_literal => |t| t.span,
        .for_ => |f| f.span,
        .undefined => |u| u.span,
    };
}
