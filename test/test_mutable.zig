const std = @import("std");
const orca = @import("orca");

test "tokenize mutable binding" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> i32 {
        \\    mut x: i32 = 0
        \\    x += 1
        \\    x
        \\}
    ;
    const actual = try orca.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(keyword fn)
        \\(symbol start)
        \\(delimiter '(')
        \\(delimiter ')')
        \\(operator ->)
        \\(symbol i32)
        \\(delimiter '{')
        \\(new_line)
        \\(keyword mut)
        \\(symbol x)
        \\(operator :)
        \\(symbol i32)
        \\(operator =)
        \\(int 0)
        \\(new_line)
        \\(symbol x)
        \\(operator +=)
        \\(int 1)
        \\(new_line)
        \\(symbol x)
        \\(new_line)
        \\(delimiter '}')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse mutable binding" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> i32 {
        \\    mut x: i32 = 0
        \\    x += 1
        \\    x
        \\}
    ;
    const actual = try orca.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(fn start [] i32
        \\    (block
        \\        (def mut x i32 0)
        \\        (+= x 1)
        \\        x))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer mutable binding" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> i32 {
        \\    mut x: i32 = 0
        \\    x += 1
        \\    x
        \\}
    ;
    const actual = try orca.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = start, type = fn() -> i32 }
        \\    return_type = i32
        \\    body =
        \\        define =
        \\            name = symbol{ value = x, type = i32 }
        \\            type = void
        \\            mutable = true
        \\            value =
        \\                int{ value = 0, type = i32 }
        \\        plus_equal =
        \\            name = symbol{ value = x, type = i32 }
        \\            type = void
        \\            value =
        \\                int{ value = 1, type = i32 }
        \\        symbol{ value = x, type = i32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen plus equal" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> i32 {
        \\    mut x: i32 = 0
        \\    x += 1
        \\    x
        \\}
    ;
    const actual = try orca.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $start (result i32)
        \\        (local $x i32)
        \\        (local.set $x
        \\            (i32.const 0))
        \\        (local.set $x
        \\            (i32.add
        \\                (local.get $x)
        \\                (i32.const 1)))
        \\        (local.get $x))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen times equal" {
    const allocator = std.testing.allocator;
    const source =
        \\fn start() -> i32 {
        \\    mut x: i32 = 5
        \\    x *= 2
        \\    x
        \\}
    ;
    const actual = try orca.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $start (result i32)
        \\        (local $x i32)
        \\        (local.set $x
        \\            (i32.const 5))
        \\        (local.set $x
        \\            (i32.mul
        \\                (local.get $x)
        \\                (i32.const 2)))
        \\        (local.get $x))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse mutable parameter" {
    const allocator = std.testing.allocator;
    const source =
        \\fn double(mut x: i32) -> void {
        \\    x *= 2
        \\}
        \\
        \\fn start() -> i32 {
        \\    mut x: i32 = 5
        \\    double(mut x)
        \\    x
        \\}
    ;
    const actual = try orca.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(fn double [(mut x i32)] void
        \\    (*= x 2))
        \\
        \\(fn start [] i32
        \\    (block
        \\        (def mut x i32 5)
        \\        (double (mut x))
        \\        x))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer mutable parameter" {
    const allocator = std.testing.allocator;
    const source =
        \\fn double(mut x: i32) -> void {
        \\    x *= 2
        \\}
        \\
        \\fn start() -> i32 {
        \\    mut x: i32 = 5
        \\    double(mut x)
        \\    x
        \\}
    ;
    const actual = try orca.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = double, type = fn(mut x: i32) -> void }
        \\    parameters =
        \\        mut symbol{ value = x, type = i32 }
        \\    return_type = void
        \\    body =
        \\        times_equal =
        \\            name = symbol{ value = x, type = i32 }
        \\            type = void
        \\            value =
        \\                int{ value = 2, type = i32 }
        \\
        \\function =
        \\    name = symbol{ value = start, type = fn() -> i32 }
        \\    return_type = i32
        \\    body =
        \\        define =
        \\            name = symbol{ value = x, type = i32 }
        \\            type = void
        \\            mutable = true
        \\            value =
        \\                int{ value = 5, type = i32 }
        \\        call =
        \\            function = symbol{ value = double, type = fn(mut x: i32) -> void }
        \\            arguments =
        \\                argument =
        \\                    mutable = true
        \\                    value = symbol{ value = x, type = i32 }
        \\            type = void
        \\        symbol{ value = x, type = i32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen mutable parameter" {
    const allocator = std.testing.allocator;
    const source =
        \\fn double(mut x: i32) -> void {
        \\    x *= 2
        \\}
        \\
        \\fn start() -> i32 {
        \\    mut x: i32 = 5
        \\    double(mut x)
        \\    x
        \\}
    ;
    const actual = try orca.testing.codegen(allocator, source);
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
        \\    (func $double (param $x/ptr i32)
        \\        (local $x i32)
        \\        (local.set $x
        \\            (i32.load
        \\                (local.get $x/ptr)))
        \\        (local.set $x
        \\            (i32.mul
        \\                (local.get $x)
        \\                (i32.const 2)))
        \\        (i32.store
        \\            (local.get $x/ptr)
        \\            (local.get $x)))
        \\
        \\    (func $start (result i32)
        \\        (local $x i32)
        \\        (local $x/ptr i32)
        \\        (local.set $x/ptr
        \\            (call $core/alloc
        \\                (i32.const 4)))
        \\        (local.set $x
        \\            (i32.const 5))
        \\        (i32.store
        \\            (local.get $x/ptr)
        \\            (local.get $x))
        \\        (call $double
        \\            (local.get $x/ptr))
        \\        (local.set $x
        \\            (i32.load
        \\                (local.get $x/ptr)))
        \\        (local.get $x))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
