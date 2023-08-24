const std = @import("std");
const atom = @import("atom");

test "tokenize array" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> []i32 {
        \\    [1, 2, 3]
        \\}
    ;
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(keyword fn)
        \\(symbol start)
        \\(delimiter '(')
        \\(delimiter ')')
        \\(operator ->)
        \\(delimiter '[')
        \\(delimiter ']')
        \\(symbol i32)
        \\(delimiter '{')
        \\(new_line)
        \\(delimiter '[')
        \\(int 1)
        \\(delimiter ',')
        \\(int 2)
        \\(delimiter ',')
        \\(int 3)
        \\(delimiter ']')
        \\(new_line)
        \\(delimiter '}')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse array" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> []i32 {
        \\    [1, 2, 3]
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(fn start [] (array i32)
        \\    [
        \\        1
        \\        2
        \\        3])
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer array" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> []i32 {
        \\    [1, 2, 3]
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = start, type = fn() -> []i32 }
        \\    return_type = []i32
        \\    body =
        \\        array =
        \\            expressions =
        \\                int{ value = 1, type = i32 }
        \\                int{ value = 2, type = i32 }
        \\                int{ value = 3, type = i32 }
        \\            type = []i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen array" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> []i32 {
        \\    [1, 2, 3]
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (global $core/arena (mut i32) (i32.const 0))
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
        \\        (local $0 i32)
        \\        (local $1 i32)
        \\        (local.set $0
        \\            (call $core/alloc
        \\                (i32.const 12)))
        \\        (local.set $1
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (block (result i32)
        \\            (i32.store
        \\                (local.get $0)
        \\                (i32.const 1))
        \\            (i32.store
        \\                (i32.add
        \\                    (local.get $0)
        \\                    (i32.const 4))
        \\                (i32.const 2))
        \\            (i32.store
        \\                (i32.add
        \\                    (local.get $0)
        \\                    (i32.const 8))
        \\                (i32.const 3))
        \\            (i32.store
        \\                (local.get $1)
        \\                (local.get $0))
        \\            (i32.store
        \\                (i32.add
        \\                    (local.get $1)
        \\                    (i32.const 4))
        \\                (i32.const 3))
        \\            (local.get $1)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse array index" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> i32 {
        \\    xs = [1, 2, 3]
        \\    xs[1]
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(fn start [] i32
        \\    (block
        \\        (def xs [
        \\            1
        \\            2
        \\            3])
        \\        (index xs 1)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer array index" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> i32 {
        \\    xs = [1, 2, 3]
        \\    xs[1]
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = start, type = fn() -> i32 }
        \\    return_type = i32
        \\    body =
        \\        define =
        \\            name = symbol{ value = xs, type = []i32 }
        \\            type = void
        \\            mutable = false
        \\            value =
        \\                array =
        \\                    expressions =
        \\                        int{ value = 1, type = i32 }
        \\                        int{ value = 2, type = i32 }
        \\                        int{ value = 3, type = i32 }
        \\                    type = []i32
        \\        index =
        \\            expression = symbol{ value = xs, type = []i32 }
        \\            indices =
        \\                int{ value = 1, type = u32 }
        \\            type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen array index" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> i32 {
        \\    xs = [1, 2, 3]
        \\    xs[1]
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (global $core/arena (mut i32) (i32.const 0))
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
        \\        (local $xs i32)
        \\        (local $0 i32)
        \\        (local $1 i32)
        \\        (local.set $0
        \\            (call $core/alloc
        \\                (i32.const 12)))
        \\        (local.set $1
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (local.set $xs
        \\            (block (result i32)
        \\                (i32.store
        \\                    (local.get $0)
        \\                    (i32.const 1))
        \\                (i32.store
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 4))
        \\                    (i32.const 2))
        \\                (i32.store
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 8))
        \\                    (i32.const 3))
        \\                (i32.store
        \\                    (local.get $1)
        \\                    (local.get $0))
        \\                (i32.store
        \\                    (i32.add
        \\                        (local.get $1)
        \\                        (i32.const 4))
        \\                    (i32.const 3))
        \\                (local.get $1)))
        \\        (if (result i32)
        \\            (i32.ge_u
        \\                (i32.const 1)
        \\                (i32.load
        \\                    (i32.add
        \\                        (local.get $xs)
        \\                        (i32.const 4))))
        \\            (then
        \\                (unreachable))
        \\            (else
        \\                (i32.load
        \\                    (i32.add
        \\                        (i32.load
        \\                            (local.get $xs))
        \\                        (i32.mul
        \\                            (i32.const 1)
        \\                            (i32.const 4)))))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen array index of string" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> u8 {
        \\    xs = "hello world"
        \\    xs[3]
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "hello world")
        \\
        \\    (global $core/arena (mut i32) (i32.const 11))
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
        \\        (local $xs i32)
        \\        (local.set $xs
        \\            (call $str
        \\                (i32.const 0)
        \\                (i32.const 11)))
        \\        (if (result i32)
        \\            (i32.ge_u
        \\                (i32.const 3)
        \\                (i32.load
        \\                    (i32.add
        \\                        (local.get $xs)
        \\                        (i32.const 4))))
        \\            (then
        \\                (unreachable))
        \\            (else
        \\                (i32.load8_u
        \\                    (i32.add
        \\                        (i32.load
        \\                            (local.get $xs))
        \\                        (i32.mul
        \\                            (i32.const 3)
        \\                            (i32.const 1)))))))
        \\
        \\    (func $str (param $ptr i32) (param $len i32) (result i32)
        \\        (local $0 i32)
        \\        (local.set $0
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $0)
        \\            (local.get $ptr))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $0)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $0))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen array of bool" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> bool {
        \\    xs = [true, false, true]
        \\    xs[1]
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (global $core/arena (mut i32) (i32.const 0))
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
        \\        (local $xs i32)
        \\        (local $0 i32)
        \\        (local $1 i32)
        \\        (local.set $0
        \\            (call $core/alloc
        \\                (i32.const 3)))
        \\        (local.set $1
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (local.set $xs
        \\            (block (result i32)
        \\                (i32.store8
        \\                    (local.get $0)
        \\                    (i32.const 1))
        \\                (i32.store8
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 1))
        \\                    (i32.const 0))
        \\                (i32.store8
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 2))
        \\                    (i32.const 1))
        \\                (i32.store
        \\                    (local.get $1)
        \\                    (local.get $0))
        \\                (i32.store
        \\                    (i32.add
        \\                        (local.get $1)
        \\                        (i32.const 4))
        \\                    (i32.const 3))
        \\                (local.get $1)))
        \\        (if (result i32)
        \\            (i32.ge_u
        \\                (i32.const 1)
        \\                (i32.load
        \\                    (i32.add
        \\                        (local.get $xs)
        \\                        (i32.const 4))))
        \\            (then
        \\                (unreachable))
        \\            (else
        \\                (i32.load8_u
        \\                    (i32.add
        \\                        (i32.load
        \\                            (local.get $xs))
        \\                        (i32.mul
        \\                            (i32.const 1)
        \\                            (i32.const 1)))))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen array of i64" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> i64 {
        \\    xs = [3, 7, 11]
        \\    xs[1]
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (global $core/arena (mut i32) (i32.const 0))
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
        \\    (func $start (result i64)
        \\        (local $xs i32)
        \\        (local $0 i32)
        \\        (local $1 i32)
        \\        (local.set $0
        \\            (call $core/alloc
        \\                (i32.const 24)))
        \\        (local.set $1
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (local.set $xs
        \\            (block (result i32)
        \\                (i64.store
        \\                    (local.get $0)
        \\                    (i64.const 3))
        \\                (i64.store
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 8))
        \\                    (i64.const 7))
        \\                (i64.store
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 16))
        \\                    (i64.const 11))
        \\                (i32.store
        \\                    (local.get $1)
        \\                    (local.get $0))
        \\                (i32.store
        \\                    (i32.add
        \\                        (local.get $1)
        \\                        (i32.const 4))
        \\                    (i32.const 3))
        \\                (local.get $1)))
        \\        (if (result i64)
        \\            (i32.ge_u
        \\                (i32.const 1)
        \\                (i32.load
        \\                    (i32.add
        \\                        (local.get $xs)
        \\                        (i32.const 4))))
        \\            (then
        \\                (unreachable))
        \\            (else
        \\                (i64.load
        \\                    (i32.add
        \\                        (i32.load
        \\                            (local.get $xs))
        \\                        (i32.mul
        \\                            (i32.const 1)
        \\                            (i32.const 8)))))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen array of f32" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> f32 {
        \\    xs = [3.14, 2.718, 1.618]
        \\    xs[1]
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (global $core/arena (mut i32) (i32.const 0))
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
        \\    (func $start (result f32)
        \\        (local $xs i32)
        \\        (local $0 i32)
        \\        (local $1 i32)
        \\        (local.set $0
        \\            (call $core/alloc
        \\                (i32.const 12)))
        \\        (local.set $1
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (local.set $xs
        \\            (block (result i32)
        \\                (f32.store
        \\                    (local.get $0)
        \\                    (f32.const 3.14000010e+00))
        \\                (f32.store
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 4))
        \\                    (f32.const 2.71799993e+00))
        \\                (f32.store
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 8))
        \\                    (f32.const 1.61800003e+00))
        \\                (i32.store
        \\                    (local.get $1)
        \\                    (local.get $0))
        \\                (i32.store
        \\                    (i32.add
        \\                        (local.get $1)
        \\                        (i32.const 4))
        \\                    (i32.const 3))
        \\                (local.get $1)))
        \\        (if (result f32)
        \\            (i32.ge_u
        \\                (i32.const 1)
        \\                (i32.load
        \\                    (i32.add
        \\                        (local.get $xs)
        \\                        (i32.const 4))))
        \\            (then
        \\                (unreachable))
        \\            (else
        \\                (f32.load
        \\                    (i32.add
        \\                        (i32.load
        \\                            (local.get $xs))
        \\                        (i32.mul
        \\                            (i32.const 1)
        \\                            (i32.const 4)))))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen array of f64" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> f64 {
        \\    xs = [3.14, 2.718, 1.618]
        \\    xs[1]
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (global $core/arena (mut i32) (i32.const 0))
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
        \\    (func $start (result f64)
        \\        (local $xs i32)
        \\        (local $0 i32)
        \\        (local $1 i32)
        \\        (local.set $0
        \\            (call $core/alloc
        \\                (i32.const 24)))
        \\        (local.set $1
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (local.set $xs
        \\            (block (result i32)
        \\                (f64.store
        \\                    (local.get $0)
        \\                    (f64.const 3.14e+00))
        \\                (f64.store
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 8))
        \\                    (f64.const 2.718e+00))
        \\                (f64.store
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 16))
        \\                    (f64.const 1.618e+00))
        \\                (i32.store
        \\                    (local.get $1)
        \\                    (local.get $0))
        \\                (i32.store
        \\                    (i32.add
        \\                        (local.get $1)
        \\                        (i32.const 4))
        \\                    (i32.const 3))
        \\                (local.get $1)))
        \\        (if (result f64)
        \\            (i32.ge_u
        \\                (i32.const 1)
        \\                (i32.load
        \\                    (i32.add
        \\                        (local.get $xs)
        \\                        (i32.const 4))))
        \\            (then
        \\                (unreachable))
        \\            (else
        \\                (f64.load
        \\                    (i32.add
        \\                        (i32.load
        \\                            (local.get $xs))
        \\                        (i32.mul
        \\                            (i32.const 1)
        \\                            (i32.const 8)))))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen array of u8" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> u8 {
        \\    xs = [2, 4, 7]
        \\    xs[1]
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (global $core/arena (mut i32) (i32.const 0))
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
        \\        (local $xs i32)
        \\        (local $0 i32)
        \\        (local $1 i32)
        \\        (local.set $0
        \\            (call $core/alloc
        \\                (i32.const 3)))
        \\        (local.set $1
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (local.set $xs
        \\            (block (result i32)
        \\                (i32.store8
        \\                    (local.get $0)
        \\                    (i32.const 2))
        \\                (i32.store8
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 1))
        \\                    (i32.const 4))
        \\                (i32.store8
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 2))
        \\                    (i32.const 7))
        \\                (i32.store
        \\                    (local.get $1)
        \\                    (local.get $0))
        \\                (i32.store
        \\                    (i32.add
        \\                        (local.get $1)
        \\                        (i32.const 4))
        \\                    (i32.const 3))
        \\                (local.get $1)))
        \\        (if (result i32)
        \\            (i32.ge_u
        \\                (i32.const 1)
        \\                (i32.load
        \\                    (i32.add
        \\                        (local.get $xs)
        \\                        (i32.const 4))))
        \\            (then
        \\                (unreachable))
        \\            (else
        \\                (i32.load8_u
        \\                    (i32.add
        \\                        (i32.load
        \\                            (local.get $xs))
        \\                        (i32.mul
        \\                            (i32.const 1)
        \\                            (i32.const 1)))))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
