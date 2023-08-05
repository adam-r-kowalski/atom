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
