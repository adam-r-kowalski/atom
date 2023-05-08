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
    const source = "f(x, y, z)";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(f x y z)";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse define then call" {
    const allocator = std.testing.allocator;
    const source =
        \\double(x: i32) -> i32 = x * 2
        \\
        \\start() = double(2)
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(defn double [(x i32)] i32 (* x 2))
        \\
        \\(defn start [] (double 2))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
