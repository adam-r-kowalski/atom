const std = @import("std");
const neuron = @import("neuron");

test "tokenize function definition" {
    const allocator = std.testing.allocator;
    const source = "double = fn(x: i32) i32 { x + x }";
    const actual = try neuron.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol double
        \\equal
        \\fn
        \\left paren
        \\symbol x
        \\colon
        \\symbol i32
        \\right paren
        \\symbol i32
        \\left brace
        \\symbol x
        \\plus
        \\symbol x
        \\right brace
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize function definition with new lines and tabs" {
    const allocator = std.testing.allocator;
    const source =
        \\double = fn(x: i32) i32 {
        \\	x + x
        \\}
    ;
    const actual = try neuron.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol double
        \\equal
        \\fn
        \\left paren
        \\symbol x
        \\colon
        \\symbol i32
        \\right paren
        \\symbol i32
        \\left brace
        \\new line
        \\symbol x
        \\plus
        \\symbol x
        \\new line
        \\right brace
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse function definition" {
    const allocator = std.testing.allocator;
    const source = "double = fn(x: i32) i32 { x + x }";
    const actual = try neuron.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def double (fn [(x i32)] i32
        \\    (+ x x)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiple parameters" {
    const allocator = std.testing.allocator;
    const source = "add = fn(x: i32, y: i32) i32 { x + y }";
    const actual = try neuron.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def add (fn [(x i32) (y i32)] i32
        \\    (+ x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize multi line function" {
    const allocator = std.testing.allocator;
    const source =
        \\sum_squares = fn(x: i32, y: i32) i32 {
        \\    x_squared = x ^ 2
        \\    y_squared = y ^ 2
        \\    x_squared + y_squared
        \\}
    ;
    const actual = try neuron.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol sum_squares
        \\equal
        \\fn
        \\left paren
        \\symbol x
        \\colon
        \\symbol i32
        \\comma
        \\symbol y
        \\colon
        \\symbol i32
        \\right paren
        \\symbol i32
        \\left brace
        \\new line
        \\symbol x_squared
        \\equal
        \\symbol x
        \\caret
        \\int 2
        \\new line
        \\symbol y_squared
        \\equal
        \\symbol y
        \\caret
        \\int 2
        \\new line
        \\symbol x_squared
        \\plus
        \\symbol y_squared
        \\new line
        \\right brace
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi line function" {
    const allocator = std.testing.allocator;
    const source =
        \\sum_squares = fn(x: i32, y: i32) i32 {
        \\    x_squared = x ^ 2
        \\    y_squared = y ^ 2
        \\    x_squared + y_squared
        \\}
    ;
    const actual = try neuron.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def sum_squares (fn [(x i32) (y i32)] i32
        \\    (block
        \\        (def x_squared (^ x 2))
        \\        (def y_squared (^ y 2))
        \\        (+ x_squared y_squared))))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer function body" {
    const allocator = std.testing.allocator;
    const source = "id = fn(x: i32) i32 { x }";
    const actual = try neuron.testing.typeInfer(allocator, source, "id");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = id, type = fn(i32) i32 }
        \\    type = void
        \\    value = 
        \\        function =
        \\            parameters =
        \\                symbol{ value = x, type = i32 }
        \\            return_type = i32
        \\            body = symbol{ value = x, type = i32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
