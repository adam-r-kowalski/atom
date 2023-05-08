const std = @import("std");
const atom = @import("atom");

test "tokenize import" {
    const allocator = std.testing.allocator;
    const source = "import print(msg: str) -> unit";
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\import
        \\symbol print
        \\left paren
        \\symbol msg
        \\colon
        \\symbol str
        \\right paren
        \\arrow
        \\symbol unit
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse add then multiply" {
    const allocator = std.testing.allocator;
    const source = "import print(msg: str) -> unit";
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(import (declare print [(msg str)] unit))";
    try std.testing.expectEqualStrings(expected, actual);
}
