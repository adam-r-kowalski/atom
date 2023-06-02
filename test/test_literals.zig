const std = @import("std");
const neuron = @import("neuron");

test "tokenize int literal followed by dot" {
    const allocator = std.testing.allocator;
    const source = "2.";
    const actual = try neuron.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(int 2)
        \\(operator .)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer int literal as i32" {
    const allocator = std.testing.allocator;
    const source = "f = fn() i32 { 42 }";
    const actual = try neuron.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = f, type = fn() i32 }
        \\    type = void
        \\    value = 
        \\        function =
        \\            return_type = i32
        \\            body = int{ value = 42, type = i32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer bool literal true" {
    const allocator = std.testing.allocator;
    const source = "f = fn() bool { true }";
    const actual = try neuron.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = f, type = fn() bool }
        \\    type = void
        \\    value = 
        \\        function =
        \\            return_type = bool
        \\            body = bool{ value = true, type = bool }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer bool literal false" {
    const allocator = std.testing.allocator;
    const source = "f = fn() bool { false }";
    const actual = try neuron.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = f, type = fn() bool }
        \\    type = void
        \\    value = 
        \\        function =
        \\            return_type = bool
        \\            body = bool{ value = false, type = bool }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer int literal as f32" {
    const allocator = std.testing.allocator;
    const source = "f = fn() f32 { 42 }";
    const actual = try neuron.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = f, type = fn() f32 }
        \\    type = void
        \\    value = 
        \\        function =
        \\            return_type = f32
        \\            body = int{ value = 42, type = f32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer float literal as f32" {
    const allocator = std.testing.allocator;
    const source = "f = fn() f32 { 42.3 }";
    const actual = try neuron.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = f, type = fn() f32 }
        \\    type = void
        \\    value = 
        \\        function =
        \\            return_type = f32
        \\            body = float{ value = 42.3, type = f32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen i32 with int literal" {
    const allocator = std.testing.allocator;
    const source = "start = fn() i32 { 42 }";
    const actual = try neuron.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (func $start (result i32)
        \\        (i32.const 42))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen f32 with int literal" {
    const allocator = std.testing.allocator;
    const source = "start = fn() f32 { 42 }";
    const actual = try neuron.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (func $start (result f32)
        \\        (f32.const 4.2e+01))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen f32 with float literal" {
    const allocator = std.testing.allocator;
    const source = "start = fn() f32 { 42.5 }";
    const actual = try neuron.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (func $start (result f32)
        \\        (f32.const 4.25e+01))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
