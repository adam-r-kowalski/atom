const std = @import("std");
pub const test_function = @import("test_function.zig");
pub const test_binary_ops = @import("test_binary_ops.zig");

test "run all tests" {
    std.testing.refAllDecls(@This());
}
