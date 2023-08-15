const std = @import("std");
const Map = std.AutoHashMap;
const parser = @import("../parser.zig");
const Interned = @import("../interner.zig").Interned;
pub const Span = parser.types.Span;

pub const Void = struct { span: ?Span };
pub const U8 = struct { span: ?Span };
pub const U32 = struct { span: ?Span };
pub const I32 = struct { span: ?Span };
pub const I64 = struct { span: ?Span };
pub const F32 = struct { span: ?Span };
pub const F64 = struct { span: ?Span };
pub const Bool = struct { span: ?Span };
pub const TypeVar = struct { value: u64, span: ?Span };

pub const Parameter = struct {
    name: Interned,
    type: MonoType,
    mutable: bool,
};

pub const Function = struct {
    parameters: []const Parameter,
    return_type: *const MonoType,
    span: ?Span,
};

pub const Argument = struct {
    type: MonoType,
    mutable: bool,
};

pub const Call = struct {
    arguments: []const Argument,
    named_arguments: Map(Interned, Argument),
    return_type: *const MonoType,
    span: ?Span,
};

pub const Array = struct {
    element_type: *const MonoType,
    span: ?Span,
};

pub const Enumeration = struct {
    variants: []const Interned,
    span: ?Span,
};

pub const EnumerationInstance = struct {
    name: Interned,
    span: ?Span,
};

pub const Fields = Map(Interned, MonoType);

pub const Structure = struct {
    fields: Fields,
    order: []const Interned,
    span: ?Span,
};

pub const StructureLiteral = struct {
    fields: Fields,
    order: []const Interned,
    structure: *const MonoType,
    span: ?Span,
};

pub const MonoType = union(enum) {
    void: Void,
    u8: U8,
    u32: U32,
    i32: I32,
    i64: I64,
    f32: F32,
    f64: F64,
    bool: Bool,
    typevar: TypeVar,
    function: Function,
    call: Call,
    array: Array,
    enumeration: Enumeration,
    enumeration_instance: EnumerationInstance,
    structure: Structure,
    structure_literal: StructureLiteral,
};

pub fn span(monotype: MonoType) ?Span {
    return switch (monotype) {
        .void => |v| v.span,
        .u8 => |u| u.span,
        .u32 => |u| u.span,
        .i32 => |i| i.span,
        .i64 => |i| i.span,
        .f32 => |f| f.span,
        .f64 => |f| f.span,
        .bool => |b| b.span,
        .typevar => |t| t.span,
        .function => |f| f.span,
        .call => |c| c.span,
        .array => |a| a.span,
        .enumeration => |e| e.span,
        .enumeration_instance => |e| e.span,
        .structure => |s| s.span,
        .structure_literal => |s| s.span,
    };
}

pub fn withSpan(monotype: MonoType, s: Span) MonoType {
    return switch (monotype) {
        .void => .{ .void = .{ .span = s } },
        .u8 => .{ .u8 = .{ .span = s } },
        .u32 => .{ .u32 = .{ .span = s } },
        .i32 => .{ .i32 = .{ .span = s } },
        .i64 => .{ .i64 = .{ .span = s } },
        .f32 => .{ .f32 = .{ .span = s } },
        .f64 => .{ .f64 = .{ .span = s } },
        .bool => .{ .bool = .{ .span = s } },
        .typevar => |t| .{ .typevar = .{ .value = t.value, .span = s } },
        .function => |f| .{ .function = .{
            .parameters = f.parameters,
            .return_type = f.return_type,
            .span = s,
        } },
        .call => |c| .{ .call = .{
            .arguments = c.arguments,
            .named_arguments = c.named_arguments,
            .return_type = c.return_type,
            .span = s,
        } },
        .array => |a| .{ .array = .{ .element_type = a.element_type, .span = s } },
        .enumeration => |e| .{ .enumeration = .{ .variants = e.variants, .span = s } },
        .enumeration_instance => |e| .{ .enumeration_instance = .{ .name = e.name, .span = s } },
        .structure => |st| .{ .structure = .{ .fields = st.fields, .order = st.order, .span = s } },
        .structure_literal => |st| .{ .structure_literal = .{
            .fields = st.fields,
            .order = st.order,
            .structure = st.structure,
            .span = s,
        } },
    };
}
