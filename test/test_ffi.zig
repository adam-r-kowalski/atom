const std = @import("std");
const atom = @import("atom");

test "tokenize import" {
    const allocator = std.testing.allocator;
    const source =
        \\foreign_import("console", "log", fn print(msg: str) void)
    ;
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\import
        \\fn
        \\symbol print
        \\left paren
        \\symbol msg
        \\colon
        \\symbol str
        \\right paren
        \\symbol void
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse import" {
    const allocator = std.testing.allocator;
    const source =
        \\foreign_import("console", "log", fn print(msg: str) void)
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(import (defn print [(msg str)] void))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize export" {
    const allocator = std.testing.allocator;
    const source =
        \\foreign_export("double", fn(x: i32) i32 {
        \\    x * 2
        \\})
    ;
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\export
        \\fn
        \\symbol double
        \\left paren
        \\symbol x
        \\colon
        \\symbol i32
        \\right paren
        \\symbol i32
        \\equal
        \\symbol x
        \\times
        \\int 2
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse export" {
    const allocator = std.testing.allocator;
    const source =
        \\foreign_export("double", fn(x: i32) i32 {
        \\    x * 2
        \\})
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(export (defn double [(x i32)] i32 (* x 2)))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse named export" {
    const allocator = std.testing.allocator;
    const source =
        \\double = fn(x: i32) i32 {
        \\    x * 2
        \\}
        \\
        \\foreign_export("double", double)
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected = "(export (defn double [(x i32)] i32 (* x 2)))";
    try std.testing.expectEqualStrings(expected, actual);
}
