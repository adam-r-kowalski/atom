const std = @import("std");
const moose = @import("moose");

test "edit distance of transposition" {
    const allocator = std.testing.allocator;
    const distance = try moose.edit_distance.editDistance(allocator, "test", "tset");
    try std.testing.expectEqual(distance, 1);
}

test "edit distance have multiple different characters" {
    const allocator = std.testing.allocator;
    const distance = try moose.edit_distance.editDistance(allocator, "test", "asdf");
    try std.testing.expectEqual(distance, 4);
}

test "edit distance identical strings" {
    const allocator = std.testing.allocator;
    const distance = try moose.edit_distance.editDistance(allocator, "test", "test");
    try std.testing.expectEqual(distance, 0);
}

test "edit distance right string is empty" {
    const allocator = std.testing.allocator;
    const distance = try moose.edit_distance.editDistance(allocator, "test", "");
    try std.testing.expectEqual(distance, 4);
}

test "edit distance left string is empty" {
    const allocator = std.testing.allocator;
    const distance = try moose.edit_distance.editDistance(allocator, "", "test");
    try std.testing.expectEqual(distance, 4);
}

test "sort by edit distance" {
    const allocator = std.testing.allocator;
    const sorted = try moose.edit_distance.sort(allocator, "banna", &.{
        "baner",
        "apple",
        "banana",
    });
    defer allocator.free(sorted);
    try std.testing.expectEqualStrings("banana", sorted[0].text);
    try std.testing.expectEqualStrings("baner", sorted[1].text);
    try std.testing.expectEqualStrings("apple", sorted[2].text);
}
