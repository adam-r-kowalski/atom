const std = @import("std");
const goat = @import("goat");
const RED = goat.error_reporter.pretty_print.RED;
const CLEAR = goat.error_reporter.pretty_print.CLEAR;

test "use of undefined variable" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> f32 {
        \\    fib(5)
        \\}
    ;
    const actual = try goat.testing.compileErrors(allocator, source);
    defer allocator.free(actual);
    const expected = try std.fmt.allocPrint(allocator,
        \\---- {s}UNDEFINED VARIABLE{s} ---------------------------------------------------
        \\
        \\Cannot find variable `fib`.
        \\
        \\1 | fn start() -> f32 {{
        \\2 |     {s}fib{s}(5)
        \\3 | }}
        \\
        \\Maybe you want one of the following?
        \\
        \\    start
        \\
    , .{ RED, CLEAR, RED, CLEAR });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, actual);
}

test "type error of if" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start(x: i32, y: f64) -> f32 {
        \\    if x == y {
        \\        x
        \\    } else {
        \\        y
        \\    }
        \\}
    ;
    const actual = try goat.testing.compileErrors(allocator, source);
    defer allocator.free(actual);
    const expected = try std.fmt.allocPrint(allocator,
        \\---- {s}TYPE MISMATCH{s} ---------------------------------------------------
        \\
        \\Here the inferred type is i32
        \\
        \\1 | fn start(x: i32, y: f64) -> f32 {{
        \\2 |     if {s}x{s} == y {{
        \\3 |         x
        \\
        \\
        \\Here the inferred type is f64
        \\
        \\1 | fn start(x: i32, y: f64) -> f32 {{
        \\2 |     if x == {s}y{s} {{
        \\3 |         x
        \\
        \\
        \\Expected these two types to be the same.
        \\
        \\
    , .{ RED, CLEAR, RED, CLEAR, RED, CLEAR });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, actual);
}

test "type error of define" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> f32 {
        \\    x: f64 = 5
        \\    x
        \\}
    ;
    const actual = try goat.testing.compileErrors(allocator, source);
    defer allocator.free(actual);
    const expected = try std.fmt.allocPrint(allocator,
        \\---- {s}TYPE MISMATCH{s} ---------------------------------------------------
        \\
        \\Here the inferred type is f32
        \\
        \\1 | fn start() -> {s}f32{s} {{
        \\2 |     x: f64 = 5
        \\
        \\
        \\Here the inferred type is f64
        \\
        \\2 |     x: f64 = 5
        \\3 |     {s}x{s}
        \\4 | }}
        \\
        \\
        \\Expected these two types to be the same.
        \\
        \\
    , .{ RED, CLEAR, RED, CLEAR, RED, CLEAR });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, actual);
}

test "type type mismatch between parameter and argument" {
    const allocator = std.testing.allocator;
    const source =
        \\fn double(x: i32) -> i32 {
        \\    x * 2
        \\}
        \\
        \\fn start() -> f32 {
        \\    y: f32 = 0
        \\    double(y)
        \\}
    ;
    const actual = try goat.testing.compileErrors(allocator, source);
    defer allocator.free(actual);
    const expected = try std.fmt.allocPrint(allocator,
        \\---- {s}TYPE MISMATCH{s} ---------------------------------------------------
        \\
        \\Here the inferred type is i32
        \\
        \\1 | fn double({s}x: i32{s}) -> i32 {{
        \\2 |     x * 2
        \\
        \\
        \\Here the inferred type is f32
        \\
        \\6 |     y: f32 = 0
        \\7 |     double({s}y{s})
        \\8 | }}
        \\
        \\
        \\Expected these two types to be the same.
        \\
        \\
    , .{ RED, CLEAR, RED, CLEAR, RED, CLEAR });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, actual);
}

test "mutability mismatch between parameter and argument" {
    const allocator = std.testing.allocator;
    const source =
        \\fn double(mut x: i32) -> void {
        \\    x *= 2
        \\}
        \\
        \\fn start() -> i32 {
        \\    x: i32 = 0
        \\    double(x)
        \\    x
        \\}
    ;
    const actual = try goat.testing.compileErrors(allocator, source);
    defer allocator.free(actual);
    const expected = try std.fmt.allocPrint(allocator,
        \\---- {s}MUTABILITY MISMATCH{s} ---------------------------------------------------
        \\
        \\Here we have a mutable value
        \\
        \\1 | fn double({s}mut x: i32{s}) -> void {{
        \\2 |     x *= 2
        \\
        \\
        \\Here we have a immutable value
        \\
        \\6 |     x: i32 = 0
        \\7 |     double({s}x{s})
        \\8 |     x
        \\
        \\
        \\Expected both of these values to be mutable.
        \\
        \\
    , .{ RED, CLEAR, RED, CLEAR, RED, CLEAR });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, actual);
}

test "mutability mismatch between binding and assignment" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> i32 {
        \\    x: i32 = 0
        \\    x += 1
        \\    x
        \\}
    ;
    const actual = try goat.testing.compileErrors(allocator, source);
    defer allocator.free(actual);
    const expected = try std.fmt.allocPrint(allocator,
        \\---- {s}REASSIGNING IMMUTABLE VALUE{s} ---------------------------------------------------
        \\
        \\Cannot reassign immutable value `{s}x{s}`.
        \\
        \\2 |     x: i32 = 0
        \\3 |     {s}x{s} += 1
        \\4 |     x
        \\
        \\Perhaps you meant to make this value mutable?
        \\
        \\
    , .{ RED, CLEAR, RED, CLEAR, RED, CLEAR });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, actual);
}

test "mutability mismatch between binding and argument" {
    const allocator = std.testing.allocator;
    const source =
        \\fn double(mut x: i32) -> void {
        \\    x *= 2
        \\}
        \\
        \\fn start() -> i32 {
        \\    x: i32 = 5
        \\    double(mut x)
        \\    x
        \\}
    ;
    const actual = try goat.testing.compileErrors(allocator, source);
    defer allocator.free(actual);
    const expected = try std.fmt.allocPrint(allocator,
        \\---- {s}MUTABILITY MISMATCH{s} ---------------------------------------------------
        \\
        \\Here we have a immutable value
        \\
        \\5 | fn start() -> i32 {{
        \\6 |     {s}x{s}: i32 = 5
        \\7 |     double(mut x)
        \\
        \\
        \\Here we have a mutable value
        \\
        \\6 |     x: i32 = 5
        \\7 |     double({s}mut x{s})
        \\8 |     x
        \\
        \\
        \\Expected both of these values to be mutable.
        \\
        \\
    , .{ RED, CLEAR, RED, CLEAR, RED, CLEAR });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, actual);
}

test "undefined variable sorted by edit distance" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> f32 {
        \\    apple: f32 = 5
        \\    banana: f32 = 10
        \\    banna
        \\}
    ;
    const actual = try goat.testing.compileErrors(allocator, source);
    defer allocator.free(actual);
    const expected = try std.fmt.allocPrint(allocator,
        \\---- {s}UNDEFINED VARIABLE{s} ---------------------------------------------------
        \\
        \\Cannot find variable `banna`.
        \\
        \\3 |     banana: f32 = 10
        \\4 |     {s}banna{s}
        \\5 | }}
        \\
        \\Maybe you want one of the following?
        \\
        \\    banana
        \\    start
        \\    apple
        \\
    , .{ RED, CLEAR, RED, CLEAR });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, actual);
}
