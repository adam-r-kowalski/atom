const std = @import("std");
const neuron = @import("neuron");

test "tokenize call" {
    const allocator = std.testing.allocator;
    const source = "f(x, y, z)";
    const actual = try neuron.testing.tokenize(allocator, source);
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
    const actual = try neuron.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(f x y z)";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse call with expression" {
    const allocator = std.testing.allocator;
    const source = "f(x + y, z)";
    const actual = try neuron.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(f (+ x y) z)";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse dot call" {
    const allocator = std.testing.allocator;
    const source = "x.f(y, z)";
    const actual = try neuron.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(. x (f y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse define then call" {
    const allocator = std.testing.allocator;
    const source =
        \\double = fn(x: i32) i32 { x * 2 }
        \\
        \\start = fn() i32 { double(2) }
    ;
    const actual = try neuron.testing.parse(allocator, source);
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
        \\double = fn(x: i32) i32 { x * 2 }
        \\
        \\start = fn() i32 { double(2) }
    ;
    const actual = try neuron.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = double, type = fn(i32) i32 }
        \\    type = void
        \\    value = 
        \\        function =
        \\            parameters =
        \\                symbol{ value = x, type = i32 }
        \\            return_type = i32
        \\            body = 
        \\                binary_op =
        \\                    kind = *
        \\                    left = symbol{ value = x, type = i32 }
        \\                    right = int{ value = 2, type = i32 }
        \\                    type = i32
        \\
        \\define =
        \\    name = symbol{ value = start, type = fn() i32 }
        \\    type = void
        \\    value = 
        \\        function =
        \\            return_type = i32
        \\            body = 
        \\                call =
        \\                    symbol{ value = double, type = fn(i32) i32 }
        \\                    arguments =
        \\                        int{ value = 2, type = i32 }
        \\                    type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen define then call" {
    const allocator = std.testing.allocator;
    const source =
        \\double = fn(x: i32) i32 { x * 2 }
        \\
        \\start = fn() i32 { double(2) }
    ;
    const actual = try neuron.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
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
        \\factorial = fn(n: i32) i32 {
        \\    if n == 0 { 1 } else { n * factorial(n - 1) }
        \\}
        \\
        \\start = fn() i32 { factorial(5) }
    ;
    const actual = try neuron.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
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

test "type infer dot call" {
    const allocator = std.testing.allocator;
    const source =
        \\double = fn(x: i32) i32 { x * 2 }
        \\
        \\start = fn() i32 { 2.double() }
    ;
    const actual = try neuron.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = double, type = fn(i32) i32 }
        \\    type = void
        \\    value = 
        \\        function =
        \\            parameters =
        \\                symbol{ value = x, type = i32 }
        \\            return_type = i32
        \\            body = 
        \\                binary_op =
        \\                    kind = *
        \\                    left = symbol{ value = x, type = i32 }
        \\                    right = int{ value = 2, type = i32 }
        \\                    type = i32
        \\
        \\define =
        \\    name = symbol{ value = start, type = fn() i32 }
        \\    type = void
        \\    value = 
        \\        function =
        \\            return_type = i32
        \\            body = 
        \\                call =
        \\                    symbol{ value = double, type = fn(i32) i32 }
        \\                    arguments =
        \\                        int{ value = 2, type = i32 }
        \\                    type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
