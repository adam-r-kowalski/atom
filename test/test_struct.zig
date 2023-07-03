const std = @import("std");
const mantis = @import("mantis");

test "tokenize struct" {
    const allocator = std.testing.allocator;
    const source =
        \\Person = struct {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\start = fn() Person {
        \\    {
        \\        name: "Bob",
        \\        age: 42,
        \\    }
        \\}
    ;
    const actual = try mantis.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol Person)
        \\(operator =)
        \\(symbol struct)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol name)
        \\(operator :)
        \\(symbol str)
        \\(delimiter ',')
        \\(new_line)
        \\(symbol age)
        \\(operator :)
        \\(symbol u8)
        \\(delimiter ',')
        \\(new_line)
        \\(delimiter '}')
        \\(new_line)
        \\(symbol start)
        \\(operator =)
        \\(keyword fn)
        \\(delimiter '(')
        \\(delimiter ')')
        \\(symbol Person)
        \\(delimiter '{')
        \\(new_line)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol name)
        \\(operator :)
        \\(string "Bob")
        \\(delimiter ',')
        \\(new_line)
        \\(symbol age)
        \\(operator :)
        \\(int 42)
        \\(delimiter ',')
        \\(new_line)
        \\(delimiter '}')
        \\(new_line)
        \\(delimiter '}')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
