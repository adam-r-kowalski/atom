const std = @import("std");
const List = std.ArrayList;
const Allocator = std.mem.Allocator;

const Interned = @import("../interner.zig").Interned;
const type_checker = @import("../type_checker.zig");

pub const UndefinedVariable = struct {
    symbol: Interned,
    span: type_checker.types.Span,
    in_scope: []const Interned,
};

pub const TypeMismatch = struct {
    left: type_checker.types.TypedSpan,
    right: type_checker.types.TypedSpan,
};

pub const Errors = struct {
    allocator: Allocator,
    undefined_variables: List(UndefinedVariable),
    type_mismatches: List(TypeMismatch),
    source: []const u8,
};
