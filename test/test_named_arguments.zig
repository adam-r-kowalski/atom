const std = @import("std");
const goat = @import("goat");

test "tokenize call with named arguments" {
    const allocator = std.testing.allocator;
    const source = "clamp(value=5, low=0, high=10)";
    const actual = try goat.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol clamp)
        \\(delimiter '(')
        \\(symbol value)
        \\(operator =)
        \\(int 5)
        \\(delimiter ',')
        \\(symbol low)
        \\(operator =)
        \\(int 0)
        \\(delimiter ',')
        \\(symbol high)
        \\(operator =)
        \\(int 10)
        \\(delimiter ')')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse call with named arguments" {
    const allocator = std.testing.allocator;
    const source = "clamp(value=5, low=0, high=10)";
    const actual = try goat.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(clamp :value 5 :low 0 :high 10)";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type check call with named arguments" {
    const allocator = std.testing.allocator;
    const source =
        \\clamp = (value: i32, low: i32, high: i32) i32 {
        \\    value
        \\}
        \\
        \\start = () i32 {
        \\    clamp(value=5, low=0, high=10)
        \\}
    ;
    const actual = try goat.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = clamp, type = fn(value: i32, low: i32, high: i32) -> i32 }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            parameters =
        \\                symbol{ value = value, type = i32 }
        \\                symbol{ value = low, type = i32 }
        \\                symbol{ value = high, type = i32 }
        \\            return_type = i32
        \\            body =
        \\                symbol{ value = value, type = i32 }
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
        \\                    function = symbol{ value = clamp, type = fn(value: i32, low: i32, high: i32) -> i32 }
        \\                    named_arguments =
        \\                        argument =
        \\                            name = value
        \\                            mutable = false
        \\                            value = int{ value = 5, type = i32 }
        \\                        argument =
        \\                            name = low
        \\                            mutable = false
        \\                            value = int{ value = 0, type = i32 }
        \\                        argument =
        \\                            name = high
        \\                            mutable = false
        \\                            value = int{ value = 10, type = i32 }
        \\                    type = i32
    ;

    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen call with named arguments" {
    const allocator = std.testing.allocator;
    const source =
        \\clamp = (value: i32, low: i32, high: i32) i32 {
        \\    value
        \\}
        \\
        \\start = () i32 {
        \\    clamp(value=5, low=0, high=10)
        \\}
    ;
    const actual = try goat.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $clamp (param $value i32) (param $low i32) (param $high i32) (result i32)
        \\        (local.get $value))
        \\
        \\    (func $start (result i32)
        \\        (call $clamp
        \\            (i32.const 5)
        \\            (i32.const 0)
        \\            (i32.const 10)))
        \\
        \\    (export "_start" (func $start)))
    ;

    try std.testing.expectEqualStrings(expected, actual);
}
