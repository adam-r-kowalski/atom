const std = @import("std");
const atom = @import("atom");

test "tokenize function definition" {
    const allocator = std.testing.allocator;
    const source = "fn double(x: i32) -> i32 = x + x";
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\fn
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

test "parse function definition" {
    const allocator = std.testing.allocator;
    const source = "fn double(x: i32) -> i32 = x + x";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(defn double [(x i32)] i32 (+ x x))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiple parameters" {
    const allocator = std.testing.allocator;
    const source = "fn add(x: i32, y: i32) -> i32 = x + y";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(defn add [(x i32) (y i32)] i32 (+ x y))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi line function" {
    const allocator = std.testing.allocator;
    const source =
        \\fn sum_squares(x: i32, y: i32) -> i32 =
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

test "type infer function body" {
    const allocator = std.testing.allocator;
    const source = "fn id(x: i32) -> i32 = x";
    const actual = try atom.testing.typeInfer(allocator, source, "id");
    defer allocator.free(actual);
    const expected =
        \\function
        \\    name = id
        \\    parameters =
        \\        symbol{ name = x, type = i32 }
        \\    return_type = i32
        \\    body = symbol{ name = x, type = i32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
