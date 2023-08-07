const std = @import("std");
const goat = @import("goat");

test "type infer convert i32 to f32" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start(x: i32) -> f32 {
        \\    convert(x, f32)
        \\}
    ;
    const actual = try goat.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = start, type = fn(x: i32) -> f32 }
        \\    parameters =
        \\        symbol{ value = x, type = i32 }
        \\    return_type = f32
        \\    body =
        \\        convert =
        \\            value =
        \\                symbol{ value = x, type = i32 }
        \\            type = f32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen convert i32 to f32" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start(x: i32) -> f32 {
        \\    convert(x, f32)
        \\}
    ;
    const actual = try goat.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $start (param $x i32) (result f32)
        \\        (f32.convert_i32_s
        \\            (local.get $x)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen convert f32 to i32" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start(x: f32) -> i32 {
        \\    convert(x, i32)
        \\}
    ;
    const actual = try goat.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $start (param $x f32) (result i32)
        \\        (i32.trunc_f32_s
        \\            (local.get $x)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen convert i64 to f64" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start(x: i64) -> f64 {
        \\    convert(x, f64)
        \\}
    ;
    const actual = try goat.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $start (param $x i64) (result f64)
        \\        (f64.convert_i64_s
        \\            (local.get $x)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen convert f64 to i64" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start(x: f64) -> i64 {
        \\    convert(x, i64)
        \\}
    ;
    const actual = try goat.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $start (param $x f64) (result i64)
        \\        (i64.trunc_f64_s
        \\            (local.get $x)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
