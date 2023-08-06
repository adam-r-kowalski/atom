const std = @import("std");
const goat = @import("goat");

test "tokenize call" {
    const allocator = std.testing.allocator;
    const source = "f(x, y, z)";
    const actual = try goat.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol f)
        \\(delimiter '(')
        \\(symbol x)
        \\(delimiter ',')
        \\(symbol y)
        \\(delimiter ',')
        \\(symbol z)
        \\(delimiter ')')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse call" {
    const allocator = std.testing.allocator;
    const source = "f(x, y, z)";
    const actual = try goat.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(f x y z)";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse call with expression" {
    const allocator = std.testing.allocator;
    const source = "f(x + y, z)";
    const actual = try goat.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(f (+ x y) z)";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse pipeline call" {
    const allocator = std.testing.allocator;
    const source = "x |> f(y, z)";
    const actual = try goat.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(|> x (f y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse define then call" {
    const allocator = std.testing.allocator;
    const source =
        \\double = (x: i32) i32 { x * 2 }
        \\
        \\start = () i32 { double(2) }
    ;
    const actual = try goat.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def double (fn [(x i32)] i32
        \\    (* x 2)))
        \\
        \\(def start (fn [] i32
        \\    (double 2)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer define then call" {
    const allocator = std.testing.allocator;
    const source =
        \\double = (x: i32) i32 { x * 2 }
        \\
        \\start = () i32 { double(2) }
    ;
    const actual = try goat.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = double, type = fn(x: i32) -> i32 }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            parameters =
        \\                symbol{ value = x, type = i32 }
        \\            return_type = i32
        \\            body =
        \\                binary_op =
        \\                    kind = *
        \\                    left =
        \\                        symbol{ value = x, type = i32 }
        \\                    right =
        \\                        int{ value = 2, type = i32 }
        \\                    type = i32
        \\
        \\define =
        \\    name = symbol{ value = start, type = fn() -> i32 }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            return_type = i32
        \\            body =
        \\                call =
        \\                    function = symbol{ value = double, type = fn(x: i32) -> i32 }
        \\                    arguments =
        \\                        argument =
        \\                            mutable = false
        \\                            value = int{ value = 2, type = i32 }
        \\                    type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen define then call" {
    const allocator = std.testing.allocator;
    const source =
        \\double = (x: i32) i32 { x * 2 }
        \\
        \\start = () i32 { double(2) }
    ;
    const actual = try goat.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $double (param $x i32) (result i32)
        \\        (i32.mul
        \\            (local.get $x)
        \\            (i32.const 2)))
        \\
        \\    (func $start (result i32)
        \\        (call $double
        \\            (i32.const 2)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen recursive function" {
    const allocator = std.testing.allocator;
    const source =
        \\factorial = (n: i32) i32 {
        \\    if n == 0 { 1 } else { n * factorial(n - 1) }
        \\}
        \\
        \\start = () i32 { factorial(5) }
    ;
    const actual = try goat.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $factorial (param $n i32) (result i32)
        \\        (if (result i32)
        \\            (i32.eq
        \\                (local.get $n)
        \\                (i32.const 0))
        \\            (then
        \\                (i32.const 1))
        \\            (else
        \\                (i32.mul
        \\                    (local.get $n)
        \\                    (call $factorial
        \\                        (i32.sub
        \\                            (local.get $n)
        \\                            (i32.const 1)))))))
        \\
        \\    (func $start (result i32)
        \\        (call $factorial
        \\            (i32.const 5)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer pipeline call" {
    const allocator = std.testing.allocator;
    const source =
        \\double = (x: i32) i32 { x * 2 }
        \\
        \\start = () i32 { 2 |> double() }
    ;
    const actual = try goat.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = double, type = fn(x: i32) -> i32 }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            parameters =
        \\                symbol{ value = x, type = i32 }
        \\            return_type = i32
        \\            body =
        \\                binary_op =
        \\                    kind = *
        \\                    left =
        \\                        symbol{ value = x, type = i32 }
        \\                    right =
        \\                        int{ value = 2, type = i32 }
        \\                    type = i32
        \\
        \\define =
        \\    name = symbol{ value = start, type = fn() -> i32 }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            return_type = i32
        \\            body =
        \\                call =
        \\                    function = symbol{ value = double, type = fn(x: i32) -> i32 }
        \\                    arguments =
        \\                        argument =
        \\                            mutable = false
        \\                            value = int{ value = 2, type = i32 }
        \\                    type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize single line pipeline call" {
    const allocator = std.testing.allocator;
    const source = "x |> f(y)";
    const actual = try goat.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol x)
        \\(operator |>)
        \\(symbol f)
        \\(delimiter '(')
        \\(symbol y)
        \\(delimiter ')')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize multi line pipeline call" {
    const allocator = std.testing.allocator;
    const source =
        \\x
        \\    |> f(y)
    ;
    const actual = try goat.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol x)
        \\(operator |>)
        \\(symbol f)
        \\(delimiter '(')
        \\(symbol y)
        \\(delimiter ')')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse call with multiple lines" {
    const allocator = std.testing.allocator;
    const source =
        \\add = (
        \\    x: i32,
        \\    y: i32
        \\) i32 {
        \\    x + y
        \\}
        \\
        \\start = () i32 {
        \\    add(
        \\        1,
        \\        2
        \\    )
        \\}
    ;
    const actual = try goat.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def add (fn [(x i32) (y i32)] i32
        \\    (+ x y)))
        \\
        \\(def start (fn [] i32
        \\    (add 1 2)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse call with space before and after" {
    const allocator = std.testing.allocator;
    const source =
        \\add = (
        \\    x: i32
        \\    ,
        \\    y: i32
        \\) i32 {
        \\    x + y
        \\}
        \\
        \\start = () i32 {
        \\    add(
        \\        1
        \\        ,
        \\        2
        \\    )
        \\}
    ;
    const actual = try goat.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def add (fn [(x i32) (y i32)] i32
        \\    (+ x y)))
        \\
        \\(def start (fn [] i32
        \\    (add 1 2)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
