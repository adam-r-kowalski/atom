const std = @import("std");
const neuron = @import("neuron");
const RED = neuron.colors.RED;
const CLEAR = neuron.colors.CLEAR;

test "use of undefined variable" {
    const allocator = std.testing.allocator;
    const source =
        \\start = fn() f32 {
        \\    fib(5)
        \\}
    ;
    const actual = try neuron.testing.compileErrors(allocator, source);
    defer allocator.free(actual);
    const expected = try std.fmt.allocPrint(allocator,
        \\--- UNDEFINED VARIABLE ---------------------------------------------------
        \\
        \\Cannot find variable `fib`.
        \\
        \\1 | start = fn() f32 {{
        \\2 |     {s}fib{s}(5)
        \\3 | }}
        \\
        \\Maybe you want one of the following?
        \\
        \\    start
        \\
    , .{ RED, CLEAR });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, actual);
}

test "type error" {
    const allocator = std.testing.allocator;
    const source =
        \\start = fn(x: i32, y: f64) f32 {
        \\    if x == y {
        \\        x
        \\    } else {
        \\        y
        \\    }
        \\}
    ;
    const actual = try neuron.testing.compileErrors(allocator, source);
    defer allocator.free(actual);
    const expected = try std.fmt.allocPrint(allocator,
        \\--- TYPE ERROR ---------------------------------------------------
        \\
        \\Here the inferred type is i32
        \\
        \\1 | start = fn(x: i32, y: f64) f32 {{
        \\2 |     if {s}x{s} == y {{
        \\3 |         x
        \\
        \\Here the inferred type is f64
        \\
        \\1 | start = fn(x: i32, y: f64) f32 {{
        \\2 |     if x == {s}y{s} {{
        \\3 |         x
        \\
        \\Expected these two types to be the same.
        \\
        \\
    , .{ RED, CLEAR, RED, CLEAR });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, actual);
}

test "type error of define" {
    const allocator = std.testing.allocator;
    const source =
        \\start = fn() f32 {
        \\    x: f64 = 5
        \\    x
        \\}
    ;
    const actual = try neuron.testing.compileErrors(allocator, source);
    defer allocator.free(actual);
    const expected = try std.fmt.allocPrint(allocator,
        \\--- TYPE ERROR ---------------------------------------------------
        \\
        \\Here the inferred type is f32
        \\
        \\1 | start = fn() {s}f32{s} {{
        \\2 |     x: f64 = 5
        \\
        \\Here the inferred type is f64
        \\
        \\1 | start = fn() f32 {s}{{
        \\2 |     x: f64 = 5
        \\3 |     x
        \\4 | }}{s}
        \\
        \\Expected these two types to be the same.
        \\
        \\
    , .{ RED, CLEAR, RED, CLEAR });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, actual);
}
