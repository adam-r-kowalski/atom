const std = @import("std");
const atom = @import("atom");

test "tokenize if" {
    const allocator = std.testing.allocator;
    const source = "if x { y } else { z }";
    const actual = try atom.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\if
        \\symbol x
        \\left brace
        \\symbol y
        \\right brace
        \\else
        \\left brace
        \\symbol z
        \\right brace
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse if" {
    const allocator = std.testing.allocator;
    const source =
        \\f = fn(x: bool, y: i32, z: i32) i32 {
        \\    if x { y } else { z }
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def f (fn [(x bool) (y i32) (z i32)] i32
        \\    (if x
        \\        y
        \\        z)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiple line if" {
    const allocator = std.testing.allocator;
    const source =
        \\f = fn(x: bool, y: i32, z: i32) i32 {
        \\    if x {
        \\        y
        \\    } else {
        \\        z
        \\    }
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def f (fn [(x bool) (y i32) (z i32)] i32
        \\    (if x
        \\        y
        \\        z)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse if multi line then else" {
    const allocator = std.testing.allocator;
    const source =
        \\f = fn(x: bool, y: i32, z: i32) i32 {
        \\    if x {
        \\        a = y ^ 2
        \\        a * 5
        \\    } else {
        \\        z
        \\    }
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def f (fn [(x bool) (y i32) (z i32)] i32
        \\    (if x
        \\        (block
        \\            (def a (^ y 2))
        \\            (* a 5))
        \\        z)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse if then multi line else" {
    const allocator = std.testing.allocator;
    const source =
        \\f = fn(x: bool, y: i32, z: i32) i32 {
        \\    if x {
        \\        y
        \\    } else {
        \\        a = z ^ 2
        \\        a * 5
        \\    }
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def f (fn [(x bool) (y i32) (z i32)] i32
        \\    (if x
        \\        y
        \\        (block
        \\            (def a (^ z 2))
        \\            (* a 5)))))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse let on result of if then else" {
    const allocator = std.testing.allocator;
    const source =
        \\b = if x {
        \\        y
        \\    } else {
        \\        a = z ^ 2
        \\        a * 5
        \\    }
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def b (if x
        \\    y
        \\    (block
        \\        (def a (^ z 2))
        \\        (* a 5))))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse nested if then else" {
    const allocator = std.testing.allocator;
    const source =
        \\f = fn(x: i32, y: i32) i32 {
        \\    if x > y {
        \\        1
        \\    } else {
        \\        if x < y {
        \\            -1
        \\        } else {
        \\            0
        \\        }
        \\    }
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def f (fn [(x i32) (y i32)] i32
        \\    (if (> x y)
        \\        1
        \\        (if (< x y)
        \\            -1
        \\            0))))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer if then else" {
    const allocator = std.testing.allocator;
    const source =
        \\f = fn(c: bool, x: i32, y: i32) i32 {
        \\    if c { x } else { y }
        \\}
    ;
    const actual = try atom.testing.typeInfer(allocator, source, "f");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ name = f, type = fn(bool, i32, i32) i32 }
        \\    type = void
        \\    value = 
        \\        function =
        \\            parameters =
        \\                symbol{ name = c, type = bool }
        \\                symbol{ name = x, type = i32 }
        \\                symbol{ name = y, type = i32 }
        \\            return_type = i32
        \\            body = 
        \\                if =
        \\                    condition = symbol{ name = c, type = bool }
        \\                    then = symbol{ name = x, type = i32 }
        \\                    else = symbol{ name = y, type = i32 }
        \\                    type = i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen if" {
    const allocator = std.testing.allocator;
    const source =
        \\start = fn() i32 {
        \\    if true { 10 } else { 20 }
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (func $start (result i32)
        \\        (if (result i32)
        \\            (i32.const 1)
        \\            (then
        \\                (i32.const 10))
        \\            (else
        \\                (i32.const 20))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen if with void result" {
    const allocator = std.testing.allocator;
    const source =
        \\print = foreign_import("stdout", "print", fn (x: i32) void)
        \\
        \\start = fn() void {
        \\    if true { print(10) } else { print(20) }
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (import "stdout" "print" (func $print (param i32)))
        \\
        \\    (func $start
        \\        (if 
        \\            (i32.const 1)
        \\            (then
        \\                (call $print
        \\                    (i32.const 10)))
        \\            (else
        \\                (call $print
        \\                    (i32.const 20)))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen if with empty else block" {
    const allocator = std.testing.allocator;
    const source =
        \\print = foreign_import("stdout", "print", fn (x: i32) void)
        \\
        \\start = fn() void {
        \\    if true { print(10) } else { }
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (import "stdout" "print" (func $print (param i32)))
        \\
        \\    (func $start
        \\        (if 
        \\            (i32.const 1)
        \\            (then
        \\                (call $print
        \\                    (i32.const 10)))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen if with no else block" {
    const allocator = std.testing.allocator;
    const source =
        \\print = foreign_import("stdout", "print", fn (x: i32) void)
        \\
        \\start = fn() void {
        \\    if true {
        \\        print(10)
        \\    }
        \\}
    ;
    const actual = try atom.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (import "stdout" "print" (func $print (param i32)))
        \\
        \\    (func $start
        \\        (if 
        \\            (i32.const 1)
        \\            (then
        \\                (call $print
        \\                    (i32.const 10)))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi arm if" {
    const allocator = std.testing.allocator;
    const source =
        \\clamp = fn(x: i32, lb: i32, ub: i32) i32 {
        \\    if {
        \\        x < lb { lb }
        \\        x > ub { ub }
        \\        else { x }
        \\    }
        \\}
    ;
    const actual = try atom.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def clamp (fn [(x i32) (lb i32) (ub i32)] i32
        \\    (cond
        \\        (< x lb)
        \\            lb
        \\        (> x ub)
        \\            ub
        \\        else
        \\            x)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
