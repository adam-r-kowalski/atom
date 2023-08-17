const std = @import("std");
const wave = @import("wave");

test "tokenize comment" {
    const allocator = std.testing.allocator;
    const source =
        \\// this is a comment
        \\
        \\fn start() -> void {
        \\    print("hello world") // write hello world to the stdout
        \\}
    ;
    const actual = try wave.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(comment // this is a comment)
        \\(new_line)
        \\(keyword fn)
        \\(symbol start)
        \\(delimiter '(')
        \\(delimiter ')')
        \\(operator ->)
        \\(symbol void)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol print)
        \\(delimiter '(')
        \\(string "hello world")
        \\(delimiter ')')
        \\(comment // write hello world to the stdout)
        \\(new_line)
        \\(delimiter '}')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse comment" {
    const allocator = std.testing.allocator;
    const source =
        \\// comment before a function
        \\
        \\fn start() -> void { // comment before function body
        \\    print("hello world") // comment after expression
        \\    // comment after function body
        \\} // comment after function
        \\
        \\// comment after function
    ;
    const actual = try wave.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(fn start [] void
        \\    (print "hello world"))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
