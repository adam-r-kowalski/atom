const std = @import("std");
const neuron = @import("neuron");

test "tokenize function definition" {
    const allocator = std.testing.allocator;
    const source = "double = fn(x: i32) i32 { x + x }";
    const actual = try neuron.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol double)
        \\(operator =)
        \\(keyword fn)
        \\(delimiter '(')
        \\(symbol x)
        \\(operator :)
        \\(symbol i32)
        \\(delimiter ')')
        \\(symbol i32)
        \\(delimiter '{')
        \\(symbol x)
        \\(operator +)
        \\(symbol x)
        \\(delimiter '}')
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
        \\(symbol double)
        \\(operator =)
        \\(keyword fn)
        \\(delimiter '(')
        \\(symbol x)
        \\(operator :)
        \\(symbol i32)
        \\(delimiter ')')
        \\(symbol i32)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol x)
        \\(operator +)
        \\(symbol x)
        \\(new_line)
        \\(delimiter '}')
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
        \\(symbol sum_squares)
        \\(operator =)
        \\(keyword fn)
        \\(delimiter '(')
        \\(symbol x)
        \\(operator :)
        \\(symbol i32)
        \\(delimiter ',')
        \\(symbol y)
        \\(operator :)
        \\(symbol i32)
        \\(delimiter ')')
        \\(symbol i32)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol x_squared)
        \\(operator =)
        \\(symbol x)
        \\(operator ^)
        \\(int 2)
        \\(new_line)
        \\(symbol y_squared)
        \\(operator =)
        \\(symbol y)
        \\(operator ^)
        \\(int 2)
        \\(new_line)
        \\(symbol x_squared)
        \\(operator +)
        \\(symbol y_squared)
        \\(new_line)
        \\(delimiter '}')
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
