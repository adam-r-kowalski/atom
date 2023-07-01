const std = @import("std");
pub const test_binary_ops = @import("test_binary_ops.zig");
pub const test_call = @import("test_call.zig");
pub const test_comments = @import("test_comments.zig");
pub const test_compile_errors = @import("test_compile_errors.zig");
pub const test_convert = @import("test_convert.zig");
pub const test_define = @import("test_define.zig");
pub const test_edit_distance = @import("test_edit_distance.zig");
pub const test_enum = @import("test_enum.zig");
pub const test_ffi = @import("test_ffi.zig");
pub const test_function = @import("test_function.zig");
pub const test_if = @import("test_if.zig");
pub const test_intrinsic = @import("test_intrinsic.zig");
pub const test_literals = @import("test_literals.zig");
pub const test_mutable = @import("test_mutable.zig");

test "run all tests" {
    std.testing.refAllDecls(@This());
}
