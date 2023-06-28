const std = @import("std");
const mantis = @import("mantis");

test "edit distance of transposition" {
    const allocator = std.testing.allocator;
    const distance = try mantis.editDistance(allocator, "test", "tset");
    try std.testing.expectEqual(distance, 1);
}

test "edit distance have multiple different characters" {
    const allocator = std.testing.allocator;
    const distance = try mantis.editDistance(allocator, "test", "asdf");
    try std.testing.expectEqual(distance, 4);
}

test "edit distance identical strings" {
    const allocator = std.testing.allocator;
    const distance = try mantis.editDistance(allocator, "test", "test");
    try std.testing.expectEqual(distance, 0);
}

test "edit distance right string is empty" {
    const allocator = std.testing.allocator;
    const distance = try mantis.editDistance(allocator, "test", "");
    try std.testing.expectEqual(distance, 4);
}

test "edit distance left string is empty" {
    const allocator = std.testing.allocator;
    const distance = try mantis.editDistance(allocator, "", "test");
    try std.testing.expectEqual(distance, 4);
}
