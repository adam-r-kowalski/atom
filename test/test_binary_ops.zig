const std = @import("std");
const atom = @import("atom");

test "tokenize add then multiply" {
    const allocator = std.testing.allocator;
    const source = "x + y * z";
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol x
        \\plus
        \\symbol y
        \\times
        \\symbol z
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse add then multiply" {
    const allocator = std.testing.allocator;
    const source = "x + y * z";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(+ x (* y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiply then add" {
    const allocator = std.testing.allocator;
    const source = "x * y + z";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(+ (* x y) z)";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiply then grouped add" {
    const allocator = std.testing.allocator;
    const source = "x * (y + z)";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(* x (+ y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiply is left associative" {
    const allocator = std.testing.allocator;
    const source = "x * y * z";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(* (* x y) z)";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse exponentiate is right associative" {
    const allocator = std.testing.allocator;
    const source = "x ^ y ^ z";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(^ x (^ y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse greater has lower precedence then add" {
    const allocator = std.testing.allocator;
    const source = "a + b > c + d";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(> (+ a b) (+ c d))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse grouped greater" {
    const allocator = std.testing.allocator;
    const source = "a + (b > c) + d";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(+ a (+ (> b c) d))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type result of add has same type as operands" {
    const allocator = std.testing.allocator;
    const source = "add(x: i32, y: i32) = x + y";
    const actual = try atom.testing.typeInfer(allocator, source, "add");
    defer allocator.free(actual);
    const expected = "add(x: i32, y: i32) -> i32 = x + y";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type operands should have the same type as the result" {
    const allocator = std.testing.allocator;
    const source = "add(x, y) -> i32 = x + y";
    const actual = try atom.testing.typeInferVerbose(allocator, source, "add");
    defer allocator.free(actual);
    const expected = "add(x: i32, y: i32) -> i32 = x + y";
    try std.testing.expectEqualStrings(expected, actual);
}
