const std = @import("std");
const atom = @import("atom");

test "tokenize single line define" {
    const allocator = std.testing.allocator;
    const source = "x = y + z";
    const actual = try atom.testing.tokenize(allocator, source);
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
        \\x = block {
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
        \\x: i32 = block {
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

test "type infer define based on body" {
    const allocator = std.testing.allocator;
    const source =
        \\fn sum_of_squares(x: i32, y: i32) -> i32 {
        \\    a = x * x
        \\    b = y * y
        \\    a + b
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "sum_of_squares");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = sum_of_squares, type = fn(x: i32, y: i32) -> i32 }
        \\    parameters =
        \\        symbol{ value = x, type = i32 }
        \\        symbol{ value = y, type = i32 }
        \\    return_type = i32
        \\    body =
        \\        define =
        \\            name = symbol{ value = a, type = i32 }
        \\            type = void
        \\            mutable = false
        \\            value =
        \\                binary_op =
        \\                    kind = *
        \\                    left =
        \\                        symbol{ value = x, type = i32 }
        \\                    right =
        \\                        symbol{ value = x, type = i32 }
        \\                    type = i32
        \\        define =
        \\            name = symbol{ value = b, type = i32 }
        \\            type = void
        \\            mutable = false
        \\            value =
        \\                binary_op =
        \\                    kind = *
        \\                    left =
        \\                        symbol{ value = y, type = i32 }
        \\                    right =
        \\                        symbol{ value = y, type = i32 }
        \\                    type = i32
        \\        binary_op =
        \\            kind = +
        \\            left =
        \\                symbol{ value = a, type = i32 }
        \\            right =
        \\                symbol{ value = b, type = i32 }
        \\            type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse nested define" {
    const allocator = std.testing.allocator;
    const source =
        \\fn f(x: i32, y: i32) -> i32 {
        \\    a = block {
        \\        b = y * y
        \\        b + x
        \\    }
        \\    a + x
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(fn f [(x i32) (y i32)] i32
        \\    (block
        \\        (def a 
        \\            (block
        \\                (def b (* y y))
        \\                (+ b x)))
        \\        (+ a x)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer nested define" {
    const allocator = std.testing.allocator;
    const source =
        \\fn f(x: i32, y: i32) -> i32 {
        \\    a = block {
        \\        b = y * y
        \\        b + x
        \\    }
        \\    a + x
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = f, type = fn(x: i32, y: i32) -> i32 }
        \\    parameters =
        \\        symbol{ value = x, type = i32 }
        \\        symbol{ value = y, type = i32 }
        \\    return_type = i32
        \\    body =
        \\        define =
        \\            name = symbol{ value = a, type = i32 }
        \\            type = void
        \\            mutable = false
        \\            value =
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
        \\                        symbol{ value = b, type = i32 }
        \\                    right =
        \\                        symbol{ value = x, type = i32 }
        \\                    type = i32
        \\        binary_op =
        \\            kind = +
        \\            left =
        \\                symbol{ value = a, type = i32 }
        \\            right =
        \\                symbol{ value = x, type = i32 }
        \\            type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen define" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start(x: i32, y: i32) -> i32 {
        \\    a = x * x
        \\    b = y * y
        \\    a + b
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
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

test "parse drop" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> void {
        \\    _ = 5
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(fn start [] void
        \\    (drop 5))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer drop" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> void {
        \\    x: i32 = 5
        \\    _ = x
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = start, type = fn() -> void }
        \\    return_type = void
        \\    body =
        \\        define =
        \\            name = symbol{ value = x, type = i32 }
        \\            type = void
        \\            mutable = false
        \\            value =
        \\                int{ value = 5, type = i32 }
        \\        drop =
        \\            type = void
        \\            value =
        \\                symbol{ value = x, type = i32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen drop" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> void {
        \\    x: i32 = 5
        \\    _ = x
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $start
        \\        (local $x i32)
        \\        (local.set $x
        \\            (i32.const 5))
        \\        (drop
        \\            (local.get $x)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer based on return type" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> i32 {
        \\    x = 5
        \\    x
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = start, type = fn() -> i32 }
        \\    return_type = i32
        \\    body =
        \\        define =
        \\            name = symbol{ value = x, type = i32 }
        \\            type = void
        \\            mutable = false
        \\            value =
        \\                int{ value = 5, type = i32 }
        \\        symbol{ value = x, type = i32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
