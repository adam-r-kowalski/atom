const std = @import("std");
const fusion = @import("fusion");

test "tokenize" {
    const allocator = std.testing.allocator;
    const source =
        \\double = \x. x + x
    ;
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const actual = try fusion.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol double
        \\equal
        \\backslash
        \\symbol x
        \\dot
        \\symbol x
        \\plus
        \\symbol x
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
