const std = @import("std");
const atom = @import("atom");

test "tokenize for loop" {
    const allocator = std.testing.allocator;
    const source =
        \\fn double(xs: []f32) -> []f32 {
        \\    for i { 2 * xs[i] }
        \\}
    ;
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(keyword fn)
        \\(symbol double)
        \\(delimiter '(')
        \\(symbol xs)
        \\(operator :)
        \\(delimiter '[')
        \\(delimiter ']')
        \\(symbol f32)
        \\(delimiter ')')
        \\(operator ->)
        \\(delimiter '[')
        \\(delimiter ']')
        \\(symbol f32)
        \\(delimiter '{')
        \\(new_line)
        \\(keyword for)
        \\(symbol i)
        \\(delimiter '{')
        \\(int 2)
        \\(operator *)
        \\(symbol xs)
        \\(delimiter '[')
        \\(symbol i)
        \\(delimiter ']')
        \\(delimiter '}')
        \\(new_line)
        \\(delimiter '}')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse for loop" {
    const allocator = std.testing.allocator;
    const source =
        \\fn double(xs: []f32) -> []f32 {
        \\    for i { 2 * xs[i] }
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(fn double [(xs (array f32))] (array f32)
        \\    (for [i]
        \\        (* 2 (index xs i))))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
