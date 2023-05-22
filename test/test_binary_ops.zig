const std = @import("std");
const atom = @import("atom");

test "tokenize add then multiply" {
    const allocator = std.testing.allocator;
    const source = "x + y * z";
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol x
        \\plus
        \\symbol y
        \\times
        \\symbol z
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse add then multiply" {
    const allocator = std.testing.allocator;
    const source = "a = x + y * z";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(def a (+ x (* y z)))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiply then add" {
    const allocator = std.testing.allocator;
    const source = "a = x * y + z";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(def a (+ (* x y) z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiply then grouped add" {
    const allocator = std.testing.allocator;
    const source = "a = x * (y + z)";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(def a (* x (+ y z)))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiply is left associative" {
    const allocator = std.testing.allocator;
    const source = "a = x * y * z";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(def a (* (* x y) z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse exponentiate is right associative" {
    const allocator = std.testing.allocator;
    const source = "a = x ^ y ^ z";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(def a (^ x (^ y z)))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse greater has lower precedence then add" {
    const allocator = std.testing.allocator;
    const source = "e = a + b > c + d";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(def e (> (+ a b) (+ c d)))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse grouped greater" {
    const allocator = std.testing.allocator;
    const source = "e = a + (b > c) + d";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(def e (+ a (+ (> b c) d)))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer binary op add" {
    const allocator = std.testing.allocator;
    const source = "add = fn(x: i32, y: i32) i32 { x + y }";
    const actual = try atom.testing.typeInfer(allocator, source, "add");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ name = add, type = fn(i32, i32) i32 }
        \\    type = void
        \\    value = 
        \\        function
        \\            parameters =
        \\                symbol{ name = x, type = i32 }
        \\                symbol{ name = y, type = i32 }
        \\            return_type = i32
        \\            body = 
        \\                binary_op =
        \\                    kind = +
        \\                    left = symbol{ name = x, type = i32 }
        \\                    right = symbol{ name = y, type = i32 }
        \\                    type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer binary op multiply" {
    const allocator = std.testing.allocator;
    const source = "multiply = fn(x: i32, y: i32) i32 { x * y }";
    const actual = try atom.testing.typeInfer(allocator, source, "multiply");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ name = multiply, type = fn(i32, i32) i32 }
        \\    type = void
        \\    value = 
        \\        function
        \\            parameters =
        \\                symbol{ name = x, type = i32 }
        \\                symbol{ name = y, type = i32 }
        \\            return_type = i32
        \\            body = 
        \\                binary_op =
        \\                    kind = *
        \\                    left = symbol{ name = x, type = i32 }
        \\                    right = symbol{ name = y, type = i32 }
        \\                    type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer binary op multiply then add" {
    const allocator = std.testing.allocator;
    const source = "f = fn(x: i32, y: i32, z: i32) i32 { x * y + z }";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ name = f, type = fn(i32, i32, i32) i32 }
        \\    type = void
        \\    value = 
        \\        function
        \\            parameters =
        \\                symbol{ name = x, type = i32 }
        \\                symbol{ name = y, type = i32 }
        \\                symbol{ name = z, type = i32 }
        \\            return_type = i32
        \\            body = 
        \\                binary_op =
        \\                    kind = +
        \\                    left = 
        \\                        binary_op =
        \\                            kind = *
        \\                            left = symbol{ name = x, type = i32 }
        \\                            right = symbol{ name = y, type = i32 }
        \\                            type = i32
        \\                    right = symbol{ name = z, type = i32 }
        \\                    type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen binary op i32.add" {
    const allocator = std.testing.allocator;
    const source = "start = fn() i32 { 42 + 29 }";
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (func $start (result i32)
        \\        (i32.add
        \\            (i32.const 42)
        \\            (i32.const 29)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
