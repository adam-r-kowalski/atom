const std = @import("std");
const atom = @import("atom");

test "tokenize if then else" {
    const allocator = std.testing.allocator;
    const source = "if x then y else z";
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\if
        \\symbol x
        \\then
        \\symbol y
        \\else
        \\symbol z
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse if then else" {
    const allocator = std.testing.allocator;
    const source = "f(x, y, z) = if x then y else z";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(defn f [x y z] (if x y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse if then else across multiple lines" {
    const allocator = std.testing.allocator;
    const source =
        \\f(x, y, z) =
        \\    if x then
        \\        y
        \\    else
        \\        z
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(defn f [x y z] (if x y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse if multi line then else" {
    const allocator = std.testing.allocator;
    const source =
        \\f(x, y, z) =
        \\    if x then
        \\        a = y ^ 2
        \\        a * 5
        \\    else
        \\        z
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(defn f [x y z] (if x
        \\        (block
        \\            (def a (^ y 2))
        \\            (* a 5))
        \\        z))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse if then multi line else" {
    const allocator = std.testing.allocator;
    const source =
        \\f(x, y, z) =
        \\    if x then
        \\        y
        \\    else
        \\        a = z ^ 2
        \\        a * 5
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(defn f [x y z] (if x y
        \\        (block
        \\            (def a (^ z 2))
        \\            (* a 5))))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse let on result of if then else" {
    const allocator = std.testing.allocator;
    const source =
        \\b = if x then
        \\        y
        \\    else
        \\        a = z ^ 2
        \\        a * 5
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def b (if x y
        \\        (block
        \\            (def a (^ z 2))
        \\            (* a 5))))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse nested if then else" {
    const allocator = std.testing.allocator;
    const source =
        \\f(x, y) =
        \\    if x > y then 1
        \\    else if x < y then -1
        \\    else 0
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(defn f [x y] (if (> x y) 1 (if (< x y) -1 0)))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer if then else infer condition, else and return type" {
    const allocator = std.testing.allocator;
    const source = "f(c, x: i32, y) = if c then x else y";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f(c: bool, x: i32, y: i32) -> i32 = if c then x else y";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer if then else infer condition, then and return type" {
    const allocator = std.testing.allocator;
    const source = "f(c, x, y: i32) = if c then x else y";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f(c: bool, x: i32, y: i32) -> i32 = if c then x else y";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer if then else infer condition, then and else" {
    const allocator = std.testing.allocator;
    const source = "f(c, x, y) -> i32 = if c then x else y";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f(c: bool, x: i32, y: i32) -> i32 = if c then x else y";
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer fully generic if then else" {
    const allocator = std.testing.allocator;
    const source = "f(c, x, y) = if c then x else y";
    const actual = try atom.testing.typeInferVerbose(allocator, source, "f");
    defer allocator.free(actual);
    const expected = "f[A](c: bool, x: A, y: A) -> A = if c then x else y";
    try std.testing.expectEqualStrings(expected, actual);
}
