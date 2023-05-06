const std = @import("std");
const atom = @import("atom");

test "tokenize with no annotation" {
    const allocator = std.testing.allocator;
    const source = "double(x) = x + x";
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol double
        \\left paren
        \\symbol x
        \\right paren
        \\equal
        \\symbol x
        \\plus
        \\symbol x
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse with no annotation" {
    const allocator = std.testing.allocator;
    const source = "double(x) = x + x";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(defn double [x] (+ x x))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize with annotation" {
    const allocator = std.testing.allocator;
    const source = "double(x: i32) -> i32 = x + x";
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol double
        \\left paren
        \\symbol x
        \\colon
        \\symbol i32
        \\right paren
        \\arrow
        \\symbol i32
        \\equal
        \\symbol x
        \\plus
        \\symbol x
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse with annotation" {
    const allocator = std.testing.allocator;
    const source = "double(x: i32) -> i32 = x + x";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(defn double [(x i32)] i32 (+ x x))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse annotating multiple bindings" {
    const allocator = std.testing.allocator;
    const source = "add(x: i32, y: i32) -> i32 = x + y";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(defn add [(x i32) (y i32)] i32 (+ x y))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse annotating multiple bindings with no return type" {
    const allocator = std.testing.allocator;
    const source = "add(x: i32, y: i32) = x + y";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(defn add [(x i32) (y i32)] (+ x y))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiple parameters annotating only return type" {
    const allocator = std.testing.allocator;
    const source = "add(x, y) -> i32 = x + y";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(defn add [x y] i32 (+ x y))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiple parameters annotating one parameter and return type" {
    const allocator = std.testing.allocator;
    const source = "add(x: i32, y) -> i32 = x + y";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(defn add [(x i32) y] i32 (+ x y))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi line function" {
    const allocator = std.testing.allocator;
    const source =
        \\sum_squares(x: i32, y: i32) -> i32 =
        \\    x_squared = x ^ 2
        \\    y_squared = y ^ 2
        \\    x_squared + y_squared
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(defn sum_squares [(x i32) (y i32)] i32
        \\    (block
        \\        (def x_squared (^ x 2))
        \\        (def y_squared (^ y 2))
        \\        (+ x_squared y_squared)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer discover return type" {
    const allocator = std.testing.allocator;
    const source = "id(x: i32) = x";
    const actual = try atom.testing.typeInfer(allocator, source, "id");
    defer allocator.free(actual);
    const expected = "id(x: i32) -> i32 = x";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer discover parameter type" {
    const allocator = std.testing.allocator;
    const source = "id(x) -> i32 = x";
    const actual = try atom.testing.typeInfer(allocator, source, "id");
    defer allocator.free(actual);
    const expected = "id(x: i32) -> i32 = x";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer consistency check" {
    const allocator = std.testing.allocator;
    const source = "id(x: i32) -> i32 = x";
    const actual = try atom.testing.typeInfer(allocator, source, "id");
    defer allocator.free(actual);
    const expected = "id(x: i32) -> i32 = x";
    try std.testing.expectEqualStrings(expected, actual);
}
