const types = @import("types.zig");
const Expression = types.Expression;
const Span = types.Span;

pub fn span(expression: Expression) Span {
    return switch (expression) {
        .int => |e| e.span,
        .float => |e| e.span,
        .symbol => |e| e.span,
        .string => |e| e.span,
        .bool => |e| e.span,
        .define => |e| e.span,
        .function => |e| e.span,
        .prototype => |e| e.span,
        .binary_op => |e| e.span,
        .group => |e| e.span,
        .block => |e| e.span,
        .if_else => |e| e.span,
        .cond => |e| e.span,
        .call => |e| e.span,
    };
}
