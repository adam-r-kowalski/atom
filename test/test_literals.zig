const std = @import("std");
const atom = @import("atom");

test "type infer int literal as i32" {
    const allocator = std.testing.allocator;
    const source = "fn f() -> i32 = 42";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\function
        \\    name = f
        \\    return_type = i32
        \\    body = int{ value = 42, type = i32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer bool literal as bool" {
    const allocator = std.testing.allocator;
    const source = "fn f() -> bool = true";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\function
        \\    name = f
        \\    return_type = bool
        \\    body = bool{ value = true, type = bool }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer bool literal false" {
    const allocator = std.testing.allocator;
    const source = "fn f() -> bool = false";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\function
        \\    name = f
        \\    return_type = bool
        \\    body = bool{ value = false, type = bool }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer int literal as f32" {
    const allocator = std.testing.allocator;
    const source = "fn f() -> f32 = 42";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\function
        \\    name = f
        \\    return_type = f32
        \\    body = int{ value = 42, type = f32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer float literal as f32" {
    const allocator = std.testing.allocator;
    const source = "fn f() -> f32 = 42.3";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\function
        \\    name = f
        \\    return_type = f32
        \\    body = float{ value = 42.3, type = f32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "lower int literal as i32" {
    const allocator = std.testing.allocator;
    const source = "fn start() -> i32 = 42";
    const actual = try atom.testing.lowerIr(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\fn start() -> i32 =
        \\    i32 42
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
