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

test "parse annotated single line define" {
    const allocator = std.testing.allocator;
    const source = "x: i32 = y + z";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(def x i32 (+ y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize define using block" {
    const allocator = std.testing.allocator;
    const source =
        \\x = {
        \\    a = y + z
        \\    a - b
        \\}
    ;
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol x
        \\equal
        \\left brace
        \\new line
        \\symbol a
        \\equal
        \\symbol y
        \\plus
        \\symbol z
        \\new line
        \\symbol a
        \\minus
        \\symbol b
        \\new line
        \\right brace
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse define using block" {
    const allocator = std.testing.allocator;
    const source =
        \\x = {
        \\    a = y + z
        \\    a - b
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def x 
        \\    (block
        \\        (def a (+ y z))
        \\        (- a b)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi line define with type annotation" {
    const allocator = std.testing.allocator;
    const source =
        \\x: i32 = {
        \\    a: i32 = y + z
        \\    a - b
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def x i32 
        \\    (block
        \\        (def a i32 (+ y z))
        \\        (- a b)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "infer type of define based on body" {
    const allocator = std.testing.allocator;
    const source =
        \\sum_of_squares = fn(x: i32, y: i32) i32 {
        \\    a = x * x
        \\    b = y * y
        \\    a + b
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "sum_of_squares");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ name = sum_of_squares, type = fn(i32, i32) i32 }
        \\    type = void
        \\    value = 
        \\        function
        \\            parameters =
        \\                symbol{ name = x, type = i32 }
        \\                symbol{ name = y, type = i32 }
        \\            return_type = i32
        \\            body = 
        \\                define =
        \\                    name = symbol{ name = a, type = i32 }
        \\                    type = void
        \\                    value = 
        \\                        binary_op =
        \\                            kind = *
        \\                            left = symbol{ name = x, type = i32 }
        \\                            right = symbol{ name = x, type = i32 }
        \\                            type = i32
        \\                define =
        \\                    name = symbol{ name = b, type = i32 }
        \\                    type = void
        \\                    value = 
        \\                        binary_op =
        \\                            kind = *
        \\                            left = symbol{ name = y, type = i32 }
        \\                            right = symbol{ name = y, type = i32 }
        \\                            type = i32
        \\                binary_op =
        \\                    kind = +
        \\                    left = symbol{ name = a, type = i32 }
        \\                    right = symbol{ name = b, type = i32 }
        \\                    type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse nested define" {
    const allocator = std.testing.allocator;
    const source =
        \\f = fn(x: i32, y: i32) i32 {
        \\    a = {
        \\        b = y * y
        \\        b + x
        \\    }
        \\    a + x
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def f (fn [(x i32) (y i32)] i32
        \\    (block
        \\        (def a 
        \\            (block
        \\                (def b (* y y))
        \\                (+ b x)))
        \\        (+ a x))))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer nested define" {
    const allocator = std.testing.allocator;
    const source =
        \\f = fn(x: i32, y: i32) i32 {
        \\    a = {
        \\        b = y * y
        \\        b + x
        \\    }
        \\    a + x
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ name = f, type = fn(i32, i32) i32 }
        \\    type = void
        \\    value = 
        \\        function
        \\            parameters =
        \\                symbol{ name = x, type = i32 }
        \\                symbol{ name = y, type = i32 }
        \\            return_type = i32
        \\            body = 
        \\                define =
        \\                    name = symbol{ name = a, type = i32 }
        \\                    type = void
        \\                    value = 
        \\                        define =
        \\                            name = symbol{ name = b, type = i32 }
        \\                            type = void
        \\                            value = 
        \\                                binary_op =
        \\                                    kind = *
        \\                                    left = symbol{ name = y, type = i32 }
        \\                                    right = symbol{ name = y, type = i32 }
        \\                                    type = i32
        \\                        binary_op =
        \\                            kind = +
        \\                            left = symbol{ name = b, type = i32 }
        \\                            right = symbol{ name = x, type = i32 }
        \\                            type = i32
        \\                binary_op =
        \\                    kind = +
        \\                    left = symbol{ name = a, type = i32 }
        \\                    right = symbol{ name = x, type = i32 }
        \\                    type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
