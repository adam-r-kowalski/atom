const std = @import("std");
pub const test_function = @import("test_function.zig");

test "run all tests" {
    std.testing.refAllDecls(@This());
}
