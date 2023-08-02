const std = @import("std");
const atom = @import("atom");

test "tokenize comment" {
    const allocator = std.testing.allocator;
    const source =
        \\# this is a comment
        \\
        \\start = () void {
        \\    print("hello world") # write hello world to the stdout
        \\}
    ;
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(comment # this is a comment)
        \\(new_line)
        \\(symbol start)
        \\(operator =)
        \\(delimiter '(')
        \\(delimiter ')')
        \\(symbol void)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol print)
        \\(delimiter '(')
        \\(string "hello world")
        \\(delimiter ')')
        \\(comment # write hello world to the stdout)
        \\(new_line)
        \\(delimiter '}')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse comment" {
    const allocator = std.testing.allocator;
    const source =
        \\# comment before a function
        \\
        \\start = () void { # comment before function body
        \\    print("hello world") # comment after expression
        \\    # comment after function body
        \\} # comment after function
        \\
        \\# comment after function
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def start (fn [] void
        \\    (print "hello world")))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
