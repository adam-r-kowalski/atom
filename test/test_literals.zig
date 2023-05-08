const std = @import("std");
const atom = @import("atom");

test "type infer int literal as i32" {
    const allocator = std.testing.allocator;
    const source = "f() -> i32 = 42";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f() -> i32 = 42";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer int literal" {
    const allocator = std.testing.allocator;
    const source = "f() = 42";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f() -> 42 = 42";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer bool literal as i32" {
    const allocator = std.testing.allocator;
    const source = "f() -> bool = true";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f() -> bool = true";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer bool literal true" {
    const allocator = std.testing.allocator;
    const source = "f() = true";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f() -> true = true";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer bool literal false" {
    const allocator = std.testing.allocator;
    const source = "f() = false";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f() -> false = false";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer int literal as f32" {
    const allocator = std.testing.allocator;
    const source = "f() -> f32 = 42";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f() -> f32 = 42";
    try std.testing.expectEqualStrings(expected, actual);
}
