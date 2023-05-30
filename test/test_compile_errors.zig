const std = @import("std");
const neuron = @import("neuron");

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
    , .{ "\x1b[31m", "\x1b[0m" });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, actual);
}

// test "type error" {
//     const allocator = std.testing.allocator;
//     const source =
//         \\start = fn(x: i32, y: f64) f32 {
//         \\    if x == y {
//         \\        x
//         \\    } else {
//         \\        y
//         \\    }
//         \\}
//     ;
//     const actual = try neuron.testing.compileErrors(allocator, source);
//     defer allocator.free(actual);
//     const expected = try std.fmt.allocPrint(allocator,
//         \\--- UNDEFINED VARIABLE ---------------------------------------------------
//         \\
//         \\Cannot find variable `fib`.
//         \\
//         \\1 | start = fn() f32 {{
//         \\2 |     {s}fib{s}(5)
//         \\3 | }}
//         \\
//         \\Maybe you want one of the following?
//         \\
//         \\    start
//         \\
//     , .{ "\x1b[31m", "\x1b[0m" });
//     defer allocator.free(expected);
//     try std.testing.expectEqualStrings(expected, actual);
// }
