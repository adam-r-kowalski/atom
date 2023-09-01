const std = @import("std");
const atom = @import("atom");

test "tokenize for loop" {
    const allocator = std.testing.allocator;
    const source =
        \\fn double(xs: vec[f32]) -> vec[f32] {
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
        \\(symbol vec)
        \\(delimiter '[')
        \\(symbol f32)
        \\(delimiter ']')
        \\(delimiter ')')
        \\(operator ->)
        \\(symbol vec)
        \\(delimiter '[')
        \\(symbol f32)
        \\(delimiter ']')
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
        \\fn double(xs: vec[f32]) -> vec[f32] {
        \\    for i { 2 * xs[i] }
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(fn double [(xs (index vec f32))] (index vec f32)
        \\    (for [i]
        \\        (* 2 (index xs i))))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi index for loop" {
    const allocator = std.testing.allocator;
    const source =
        \\fn transpose(m: mat[f32]) -> mat[f32] {
        \\    for i, j { m[j, i] }
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(fn transpose [(m (index mat f32))] (index mat f32)
        \\    (for [i j]
        \\        (index m j i)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
