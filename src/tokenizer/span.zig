const types = @import("types.zig");
const Token = types.Token;
const Span = types.Span;

pub fn span(token: Token) Span {
    return switch (token) {
        .symbol => |t| t.span,
        .int => |t| t.span,
        .float => |t| t.span,
        .string => |t| t.span,
        .bool => |t| t.span,
        .equal => |t| t.span,
        .equal_equal => |t| t.span,
        .dot => |t| t.span,
        .colon => |t| t.span,
        .plus => |t| t.span,
        .minus => |t| t.span,
        .times => |t| t.span,
        .slash => |t| t.span,
        .caret => |t| t.span,
        .greater => |t| t.span,
        .less => |t| t.span,
        .percent => |t| t.span,
        .left_paren => |t| t.span,
        .right_paren => |t| t.span,
        .left_brace => |t| t.span,
        .right_brace => |t| t.span,
        .if_ => |t| t.span,
        .else_ => |t| t.span,
        .or_ => |t| t.span,
        .comma => |t| t.span,
        .fn_ => |t| t.span,
        .new_line => |t| t.span,
    };
}
