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
    const source = "fn add(x: i32, y: i32) i32 = x + y";
    const actual = try atom.testing.typeInfer(allocator, source, "add");
    defer allocator.free(actual);
    const expected =
        \\function
        \\    name = add
        \\    parameters =
        \\        symbol{ name = x, type = i32 }
        \\        symbol{ name = y, type = i32 }
        \\    return_type = i32
        \\    body = 
        \\        binary_op =
        \\            kind = +
        \\            left = symbol{ name = x, type = i32 }
        \\            right = symbol{ name = y, type = i32 }
        \\            type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer binary op multiply" {
    const allocator = std.testing.allocator;
    const source = "fn multiply(x: i32, y: i32) i32 = x * y";
    const actual = try atom.testing.typeInfer(allocator, source, "multiply");
    defer allocator.free(actual);
    const expected =
        \\function
        \\    name = multiply
        \\    parameters =
        \\        symbol{ name = x, type = i32 }
        \\        symbol{ name = y, type = i32 }
        \\    return_type = i32
        \\    body = 
        \\        binary_op =
        \\            kind = *
        \\            left = symbol{ name = x, type = i32 }
        \\            right = symbol{ name = y, type = i32 }
        \\            type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer binary op multiply then add" {
    const allocator = std.testing.allocator;
    const source = "fn f(x: i32, y: i32, z: i32) i32 = x * y + z";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\function
        \\    name = f
        \\    parameters =
        \\        symbol{ name = x, type = i32 }
        \\        symbol{ name = y, type = i32 }
        \\        symbol{ name = z, type = i32 }
        \\    return_type = i32
        \\    body = 
        \\        binary_op =
        \\            kind = +
        \\            left = 
        \\                binary_op =
        \\                    kind = *
        \\                    left = symbol{ name = x, type = i32 }
        \\                    right = symbol{ name = y, type = i32 }
        \\                    type = i32
        \\            right = symbol{ name = z, type = i32 }
        \\            type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
