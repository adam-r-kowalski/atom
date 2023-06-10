const std = @import("std");
const mantis = @import("mantis");

test "tokenize single line define" {
    const allocator = std.testing.allocator;
    const source = "x = y + z";
    const actual = try mantis.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol x)
        \\(operator =)
        \\(symbol y)
        \\(operator +)
        \\(symbol z)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse single line define" {
    const allocator = std.testing.allocator;
    const source = "x = y + z";
    const actual = try mantis.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(def x (+ y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize annotated single line define" {
    const allocator = std.testing.allocator;
    const source = "x: i32 = y + z";
    const actual = try mantis.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol x)
        \\(operator :)
        \\(symbol i32)
        \\(operator =)
        \\(symbol y)
        \\(operator +)
        \\(symbol z)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse annotated single line define" {
    const allocator = std.testing.allocator;
    const source = "x: i32 = y + z";
    const actual = try mantis.testing.parse(allocator, source);
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
    const actual = try mantis.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol x)
        \\(operator =)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol a)
        \\(operator =)
        \\(symbol y)
        \\(operator +)
        \\(symbol z)
        \\(new_line)
        \\(symbol a)
        \\(operator -)
        \\(symbol b)
        \\(new_line)
        \\(delimiter '}')
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
    const actual = try mantis.testing.parse(allocator, source);
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
    const actual = try mantis.testing.parse(allocator, source);
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
    const actual = try mantis.testing.typeInfer(allocator, source, "sum_of_squares");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = sum_of_squares, type = fn(i32, i32) i32 }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            parameters =
        \\                symbol{ value = x, type = i32 }
        \\                symbol{ value = y, type = i32 }
        \\            return_type = i32
        \\            body =
        \\                define =
        \\                    name = symbol{ value = a, type = i32 }
        \\                    type = void
        \\                    mutable = false
        \\                    value =
        \\                        binary_op =
        \\                            kind = *
        \\                            left =
        \\                                symbol{ value = x, type = i32 }
        \\                            right =
        \\                                symbol{ value = x, type = i32 }
        \\                            type = i32
        \\                define =
        \\                    name = symbol{ value = b, type = i32 }
        \\                    type = void
        \\                    mutable = false
        \\                    value =
        \\                        binary_op =
        \\                            kind = *
        \\                            left =
        \\                                symbol{ value = y, type = i32 }
        \\                            right =
        \\                                symbol{ value = y, type = i32 }
        \\                            type = i32
        \\                binary_op =
        \\                    kind = +
        \\                    left =
        \\                        symbol{ value = a, type = i32 }
        \\                    right =
        \\                        symbol{ value = b, type = i32 }
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
    const actual = try mantis.testing.parse(allocator, source);
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
    const actual = try mantis.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = f, type = fn(i32, i32) i32 }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            parameters =
        \\                symbol{ value = x, type = i32 }
        \\                symbol{ value = y, type = i32 }
        \\            return_type = i32
        \\            body =
        \\                define =
        \\                    name = symbol{ value = a, type = i32 }
        \\                    type = void
        \\                    mutable = false
        \\                    value =
        \\                        define =
        \\                            name = symbol{ value = b, type = i32 }
        \\                            type = void
        \\                            mutable = false
        \\                            value =
        \\                                binary_op =
        \\                                    kind = *
        \\                                    left =
        \\                                        symbol{ value = y, type = i32 }
        \\                                    right =
        \\                                        symbol{ value = y, type = i32 }
        \\                                    type = i32
        \\                        binary_op =
        \\                            kind = +
        \\                            left =
        \\                                symbol{ value = b, type = i32 }
        \\                            right =
        \\                                symbol{ value = x, type = i32 }
        \\                            type = i32
        \\                binary_op =
        \\                    kind = +
        \\                    left =
        \\                        symbol{ value = a, type = i32 }
        \\                    right =
        \\                        symbol{ value = x, type = i32 }
        \\                    type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen define" {
    const allocator = std.testing.allocator;
    const source =
        \\start = fn(x: i32, y: i32) i32 {
        \\    a = x * x
        \\    b = y * y
        \\    a + b
        \\}
    ;
    const actual = try mantis.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\    (global $arena (mut i32) (i32.const 0))
        \\
        \\    (func $start (param $x i32) (param $y i32) (result i32)
        \\        (local $a i32)
        \\        (local $b i32)
        \\        (local.set $a
        \\            (i32.mul
        \\                (local.get $x)
        \\                (local.get $x)))
        \\        (local.set $b
        \\            (i32.mul
        \\                (local.get $y)
        \\                (local.get $y)))
        \\        (i32.add
        \\            (local.get $a)
        \\            (local.get $b)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
