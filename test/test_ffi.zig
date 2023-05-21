const std = @import("std");
const atom = @import("atom");

test "tokenize import" {
    const allocator = std.testing.allocator;
    const source =
        \\print = foreign_import("console", "log", fn(msg: str) void)
    ;
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\symbol print
        \\equal
        \\symbol foreign_import
        \\left paren
        \\string "console"
        \\comma
        \\string "log"
        \\comma
        \\fn
        \\left paren
        \\symbol msg
        \\colon
        \\symbol str
        \\right paren
        \\symbol void
        \\right paren
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse import" {
    const allocator = std.testing.allocator;
    const source =
        \\print = foreign_import("console", "log", fn(msg: str) void)
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def print (foreign_import "console" "log" (fn [(msg str)] void)))
    ;
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
        \\symbol foreign_export
        \\left paren
        \\string "double"
        \\comma
        \\fn
        \\left paren
        \\symbol x
        \\colon
        \\symbol i32
        \\right paren
        \\symbol i32
        \\left brace
        \\new line
        \\symbol x
        \\times
        \\int 2
        \\new line
        \\right brace
        \\right paren
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
    const expected =
        \\(foreign_export "double" (fn [(x i32)] i32
        \\    (* x 2)))
    ;
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
    const expected =
        \\(def double (fn [(x i32)] i32
        \\    (* x 2)))
        \\
        \\(foreign_export "double" double)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
