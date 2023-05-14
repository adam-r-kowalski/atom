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

test "type result of add has same type as operands" {
    const allocator = std.testing.allocator;
    const source = "add(x: i32, y: i32) = x + y";
    const actual = try atom.testing.typeInfer(allocator, source, "add");
    defer allocator.free(actual);
    const expected = "add(x: i32, y: i32) -> i32 = x + y";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type operands should have the same type as the result" {
    const allocator = std.testing.allocator;
    const source = "add(x, y) -> i32 = x + y";
    const actual = try atom.testing.typeInfer(allocator, source, "add");
    defer allocator.free(actual);
    const expected = "add(x: i32, y: i32) -> i32 = x + y";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer can figure out second parameter and return type" {
    const allocator = std.testing.allocator;
    const source = "add(x: i32, y) = x + y";
    const actual = try atom.testing.typeInfer(allocator, source, "add");
    defer allocator.free(actual);
    const expected = "add(x: i32, y: i32) -> i32 = x + y";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer can figure out first parameter and return type" {
    const allocator = std.testing.allocator;
    const source = "add(x, y: i32) = x + y";
    const actual = try atom.testing.typeInfer(allocator, source, "add");
    defer allocator.free(actual);
    const expected = "add(x: i32, y: i32) -> i32 = x + y";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer fully generic add" {
    const allocator = std.testing.allocator;
    const source = "add(x, y) = x + y";
    const actual = try atom.testing.typeInfer(allocator, source, "add");
    defer allocator.free(actual);
    const expected = "add[A](x: A, y: A) -> A = x + y";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer multiply" {
    const allocator = std.testing.allocator;
    const source = "mul(x: i32, y: i32) = x * y";
    const actual = try atom.testing.typeInfer(allocator, source, "mul");
    defer allocator.free(actual);
    const expected = "mul(x: i32, y: i32) -> i32 = x * y";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer multiply then add infer last two parameter types and return type" {
    const allocator = std.testing.allocator;
    const source = "f(x: i32, y, z) = x * y + z";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f(x: i32, y: i32, z: i32) -> i32 = x * y + z";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer multiply then add infer first and last parameter types and return type" {
    const allocator = std.testing.allocator;
    const source = "f(x, y: i32, z) = x * y + z";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f(x: i32, y: i32, z: i32) -> i32 = x * y + z";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer multiply then add infer first two parameter types and return type" {
    const allocator = std.testing.allocator;
    const source = "f(x, y, z: i32) = x * y + z";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f(x: i32, y: i32, z: i32) -> i32 = x * y + z";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer multiply then add infer all parameter types" {
    const allocator = std.testing.allocator;
    const source = "f(x, y, z) -> i32 = x * y + z";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f(x: i32, y: i32, z: i32) -> i32 = x * y + z";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer add then multiply infer last two parameter types and return type" {
    const allocator = std.testing.allocator;
    const source = "f(x: i32, y, z) = x + y * z";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f(x: i32, y: i32, z: i32) -> i32 = x + y * z";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer add then multiply infer first and last parameter types and return type" {
    const allocator = std.testing.allocator;
    const source = "f(x, y: i32, z) = x + y * z";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f(x: i32, y: i32, z: i32) -> i32 = x + y * z";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer add then multiply infer first two parameter types and return type" {
    const allocator = std.testing.allocator;
    const source = "f(x, y, z: i32) = x + y * z";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f(x: i32, y: i32, z: i32) -> i32 = x + y * z";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer add then multiply infer all parameter types" {
    const allocator = std.testing.allocator;
    const source = "f(x, y, z) -> i32 = x + y * z";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f(x: i32, y: i32, z: i32) -> i32 = x + y * z";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer fully generic add then multiply" {
    const allocator = std.testing.allocator;
    const source = "f(x, y, z) = x + y * z";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f[A](x: A, y: A, z: A) -> A = x + y * z";
    try std.testing.expectEqualStrings(expected, actual);
}
