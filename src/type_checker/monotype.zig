const parser = @import("../parser.zig");
pub const Span = parser.types.Span;

pub const Void = struct { span: ?Span };
pub const U8 = struct { span: ?Span };
pub const I32 = struct { span: ?Span };
pub const I64 = struct { span: ?Span };
pub const F32 = struct { span: ?Span };
pub const F64 = struct { span: ?Span };
pub const Bool = struct { span: ?Span };
pub const TypeVar = struct { value: u64, span: ?Span };

const Function = struct {
    parameters: []const MonoType,
    return_type: *const MonoType,
    span: ?Span,
};

const Array = struct {
    size: ?u32,
    element_type: *const MonoType,
    span: ?Span,
};

pub const MonoType = union(enum) {
    void: Void,
    u8: U8,
    i32: I32,
    i64: I64,
    f32: F32,
    f64: F64,
    bool: Bool,
    typevar: TypeVar,
    function: Function,
    array: Array,
};

pub fn span(monotype: MonoType) ?Span {
    return switch (monotype) {
        .void => |v| v.span,
        .u8 => |u| u.span,
        .i32 => |i| i.span,
        .i64 => |i| i.span,
        .f32 => |f| f.span,
        .f64 => |f| f.span,
        .bool => |b| b.span,
        .typevar => |t| t.span,
        .function => |f| f.span,
        .array => |a| a.span,
    };
}

pub fn withSpan(monotype: MonoType, s: Span) MonoType {
    return switch (monotype) {
        .void => .{ .void = .{ .span = s } },
        .u8 => .{ .u8 = .{ .span = s } },
        .i32 => .{ .i32 = .{ .span = s } },
        .i64 => .{ .i64 = .{ .span = s } },
        .f32 => .{ .f32 = .{ .span = s } },
        .f64 => .{ .f64 = .{ .span = s } },
        .bool => .{ .bool = .{ .span = s } },
        .typevar => |t| .{ .typevar = .{ .value = t.value, .span = s } },
        .function => |f| .{ .function = .{ .parameters = f.parameters, .return_type = f.return_type, .span = s } },
        .array => |a| .{ .array = .{ .size = a.size, .element_type = a.element_type, .span = s } },
    };
}
