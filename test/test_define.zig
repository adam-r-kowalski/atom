const std = @import("std");
const atom = @import("atom");

test "tokenize single line define" {
    const allocator = std.testing.allocator;
    const source = "x = y + z";
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol x
        \\equal
        \\symbol y
        \\plus
        \\symbol z
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse single line define" {
    const allocator = std.testing.allocator;
    const source = "x = y + z";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(def x (+ y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize annotated single line define" {
    const allocator = std.testing.allocator;
    const source = "x: i32 = y + z";
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol x
        \\colon
        \\symbol i32
        \\equal
        \\symbol y
        \\plus
        \\symbol z
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse single line define" {
    const allocator = std.testing.allocator;
    const source = "x: i32 = y + z";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(def x i32 (+ y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize multi line define" {
    const allocator = std.testing.allocator;
    const source =
        \\x =
        \\  a = y + z
        \\  a - b
    ;
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol x
        \\equal
        \\space 2
        \\symbol a
        \\equal
        \\symbol y
        \\plus
        \\symbol z
        \\space 2
        \\symbol a
        \\minus
        \\symbol b
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi line define" {
    const allocator = std.testing.allocator;
    const source =
        \\x =
        \\  a = y + z
        \\  a - b
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def x
        \\  (block
        \\    (def a (+ y z))
        \\    (- a b)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi line define with type annotation" {
    const allocator = std.testing.allocator;
    const source =
        \\x: i32 =
        \\  a: i32 = y + z
        \\  a - b
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def x i32
        \\  (block
        \\    (def a i32 (+ y z))
        \\    (- a b)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "infer type of define based on body" {
    const allocator = std.testing.allocator;
    const source =
        \\sum_of_squares(x: i32, y: i32) -> i32 =
        \\  a = x * x
        \\  b = y * y
        \\  a + b
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "sum_of_squares");
    defer allocator.free(actual);
    const expected =
        \\sum_of_squares(x: i32, y: i32) -> i32 =
        \\  a: i32 = x * x
        \\  b: i32 = y * y
        \\  a + b
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "infer parameter types and return type based on type of define" {
    const allocator = std.testing.allocator;
    const source =
        \\sum_of_squares(x, y) =
        \\  a: i32 = x * x
        \\  b = y * y
        \\  a + b
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "sum_of_squares");
    defer allocator.free(actual);
    const expected =
        \\sum_of_squares(x: i32, y: i32) -> i32 =
        \\  a: i32 = x * x
        \\  b: i32 = y * y
        \\  a + b
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer of fully generic define" {
    const allocator = std.testing.allocator;
    const source =
        \\sum_of_squares(x, y) =
        \\  a = x * x
        \\  b = y * y
        \\  a + b
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "sum_of_squares");
    defer allocator.free(actual);
    const expected =
        \\sum_of_squares[A](x: A, y: A) -> A =
        \\  a: A = x * x
        \\  b: A = y * y
        \\  a + b
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
