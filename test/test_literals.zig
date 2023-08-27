const std = @import("std");
const atom = @import("atom");

test "tokenize int literal followed by dot" {
    const allocator = std.testing.allocator;
    const source = "2.";
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(int 2)
        \\(operator .)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer int literal as i32" {
    const allocator = std.testing.allocator;
    const source = "fn f() -> i32 { 42 }";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = f, type = fn() -> i32 }
        \\    return_type = i32
        \\    body =
        \\        int{ value = 42, type = i32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer bool literal true" {
    const allocator = std.testing.allocator;
    const source = "fn f() -> bool { true }";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = f, type = fn() -> bool }
        \\    return_type = bool
        \\    body =
        \\        bool{ value = true, type = bool }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer bool literal false" {
    const allocator = std.testing.allocator;
    const source = "fn f() -> bool { false }";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = f, type = fn() -> bool }
        \\    return_type = bool
        \\    body =
        \\        bool{ value = false, type = bool }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer int literal as f32" {
    const allocator = std.testing.allocator;
    const source = "fn f() -> f32 { 42 }";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = f, type = fn() -> f32 }
        \\    return_type = f32
        \\    body =
        \\        int{ value = 42, type = f32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer float literal as f32" {
    const allocator = std.testing.allocator;
    const source = "fn f() -> f32 { 42.3 }";
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = f, type = fn() -> f32 }
        \\    return_type = f32
        \\    body =
        \\        float{ value = 42.3, type = f32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen i32 with int literal" {
    const allocator = std.testing.allocator;
    const source = "fn start() -> i32 { 42 }";
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
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
    const source = "fn start() -> f32 { 42 }";
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
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
    const source = "fn start() -> f32 { 42.5 }";
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $start (result f32)
        \\        (f32.const 4.25e+01))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen i32 global constant" {
    const allocator = std.testing.allocator;
    const source =
        \\i: i32 = 42
        \\
        \\fn start() -> i32 { i }
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (global $i i32 (i32.const 42))
        \\
        \\    (func $start (result i32)
        \\        (global.get $i))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen str with string literal" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> str { "hi" }
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "hi")
        \\
        \\    (global $core/arena (mut i32) (i32.const 2))
        \\
        \\    (func $core/alloc (param $size i32) (result i32)
        \\        (local $ptr i32)
        \\        (local.tee $ptr
        \\            (global.get $core/arena))
        \\        (global.set $core/arena
        \\            (i32.add
        \\                (local.get $ptr)
        \\                (local.get $size))))
        \\
        \\    (func $start (result i32)
        \\        (call $str
        \\            (i32.const 0)
        \\            (i32.const 2)))
        \\
        \\    (func $str (param $ptr i32) (param $len i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $result)
        \\            (local.get $ptr))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $result))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen str with template literal" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> str { `hi` }
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "hi")
        \\
        \\    (global $core/arena (mut i32) (i32.const 2))
        \\
        \\    (func $core/alloc (param $size i32) (result i32)
        \\        (local $ptr i32)
        \\        (local.tee $ptr
        \\            (global.get $core/arena))
        \\        (global.set $core/arena
        \\            (i32.add
        \\                (local.get $ptr)
        \\                (local.get $size))))
        \\
        \\    (func $start (result i32)
        \\        (call $str
        \\            (i32.const 0)
        \\            (i32.const 2)))
        \\
        \\    (func $str (param $ptr i32) (param $len i32) (result i32)
        \\        (local $result i32)
        \\        (local.set $result
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $result)
        \\            (local.get $ptr))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $result)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $result))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
