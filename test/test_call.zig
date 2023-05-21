const std = @import("std");
const atom = @import("atom");

test "tokenize call" {
    const allocator = std.testing.allocator;
    const source = "f(x, y, z)";
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol f
        \\left paren
        \\symbol x
        \\comma
        \\symbol y
        \\comma
        \\symbol z
        \\right paren
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse call" {
    const allocator = std.testing.allocator;
    const source = "a = f(x, y, z)";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(def a (f x y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse define then call" {
    const allocator = std.testing.allocator;
    const source =
        \\double = fn(x: i32) i32 { x * 2 }
        \\
        \\start = fn() i32 { double(2) }
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def double (fn [(x i32)] i32 (* x 2)))
        \\
        \\(def start (fn [] i32 (double 2)))
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
    const actual = try atom.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\function
        \\    name = double
        \\    parameters =
        \\        symbol{ name = x, type = i32 }
        \\    return_type = i32
        \\    body = 
        \\        binary_op =
        \\            kind = *
        \\            left = symbol{ name = x, type = i32 }
        \\            right = int{ value = 2, type = i32 }
        \\            type = i32
        \\
        \\function
        \\    name = start
        \\    return_type = i32
        \\    body = 
        \\        call =
        \\            symbol{ name = double, type = (i32) -> i32 }
        \\            arguments =
        \\                int{ value = 2, type = i32 }
        \\            type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
