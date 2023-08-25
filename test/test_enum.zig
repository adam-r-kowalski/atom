const std = @import("std");
const atom = @import("atom");

test "tokenize enum" {
    const allocator = std.testing.allocator;
    const source =
        \\enum Grade {
        \\    a,
        \\    b,
        \\    c,
        \\    d,
        \\    f,
        \\}
        \\
        \\fn start() -> Grade {
        \\    Grade.a
        \\}
    ;
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(keyword enum)
        \\(symbol Grade)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol a)
        \\(delimiter ',')
        \\(new_line)
        \\(symbol b)
        \\(delimiter ',')
        \\(new_line)
        \\(symbol c)
        \\(delimiter ',')
        \\(new_line)
        \\(symbol d)
        \\(delimiter ',')
        \\(new_line)
        \\(symbol f)
        \\(delimiter ',')
        \\(new_line)
        \\(delimiter '}')
        \\(new_line)
        \\(keyword fn)
        \\(symbol start)
        \\(delimiter '(')
        \\(delimiter ')')
        \\(operator ->)
        \\(symbol Grade)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol Grade)
        \\(operator .)
        \\(symbol a)
        \\(new_line)
        \\(delimiter '}')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse enum" {
    const allocator = std.testing.allocator;
    const source =
        \\enum Grade {
        \\    a,
        \\    b,
        \\    c,
        \\    d,
        \\    f,
        \\}
        \\
        \\fn start() -> Grade {
        \\    Grade.a
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(enum Grade
        \\    a
        \\    b
        \\    c
        \\    d
        \\    f)
        \\
        \\(fn start [] Grade
        \\    (. Grade a))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer enum" {
    const allocator = std.testing.allocator;
    const source =
        \\enum Grade {
        \\    a,
        \\    b,
        \\    c,
        \\    d,
        \\    f,
        \\}
        \\
        \\fn start() -> Grade {
        \\    Grade.a
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\function =
        \\    name = symbol{ value = start, type = fn() -> Grade }
        \\    return_type = Grade
        \\    body =
        \\        dot =
        \\            left =
        \\                symbol{ value = Grade, type = Grade }
        \\            right = symbol{ value = a, type = Grade }
        \\            type = Grade
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen enum index 0" {
    const allocator = std.testing.allocator;
    const source =
        \\enum Grade {
        \\    a,
        \\    b,
        \\    c,
        \\    d,
        \\    f,
        \\}
        \\
        \\fn start() -> Grade {
        \\    Grade.a
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
        \\    (func $start (result i32)
        \\        (i32.const 0))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen enum index 1" {
    const allocator = std.testing.allocator;
    const source =
        \\enum Grade {
        \\    a,
        \\    b,
        \\    c,
        \\    d,
        \\    f,
        \\}
        \\
        \\fn start() -> Grade {
        \\    Grade.b
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
        \\    (func $start (result i32)
        \\        (i32.const 1))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen enum equality" {
    const allocator = std.testing.allocator;
    const source =
        \\enum Grade {
        \\    a,
        \\    b,
        \\    c,
        \\    d,
        \\    f,
        \\}
        \\
        \\fn start() -> bool {
        \\    Grade.a == Grade.b
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
        \\    (func $start (result i32)
        \\        (i32.eq
        \\            (i32.const 0)
        \\            (i32.const 1)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen enum passed to function" {
    const allocator = std.testing.allocator;
    const source =
        \\enum Grade {
        \\    a,
        \\    b,
        \\    c,
        \\    d,
        \\    f,
        \\}
        \\
        \\fn got_an_a(grade: Grade) -> bool {
        \\    grade == Grade.a
        \\}
        \\
        \\fn start() -> bool {
        \\    got_an_a(Grade.b)
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
        \\    (func $got_an_a (param $grade i32) (result i32)
        \\        (i32.eq
        \\            (local.get $grade)
        \\            (i32.const 0)))
        \\
        \\    (func $start (result i32)
        \\        (call $got_an_a
        \\            (i32.const 1)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
