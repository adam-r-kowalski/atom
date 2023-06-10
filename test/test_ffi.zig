const std = @import("std");
const mantis = @import("mantis");

test "tokenize import" {
    const allocator = std.testing.allocator;
    const source =
        \\print = foreign_import("console", "log", fn(msg: []u8) void)
    ;
    const actual = try mantis.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol print)
        \\(operator =)
        \\(symbol foreign_import)
        \\(delimiter '(')
        \\(string "console")
        \\(delimiter ',')
        \\(string "log")
        \\(delimiter ',')
        \\(keyword fn)
        \\(delimiter '(')
        \\(symbol msg)
        \\(operator :)
        \\(delimiter '[')
        \\(delimiter ']')
        \\(symbol u8)
        \\(delimiter ')')
        \\(symbol void)
        \\(delimiter ')')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse import" {
    const allocator = std.testing.allocator;
    const source =
        \\print = foreign_import("console", "log", fn(msg: []u8) void)
    ;
    const actual = try mantis.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def print (foreign_import "console" "log" (fn [(msg []u8)] void)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type check import" {
    const allocator = std.testing.allocator;
    const source =
        \\print = foreign_import("console", "log", fn(msg: []u8) void)
        \\
        \\start = fn() void {
        \\    print("hello world")
        \\}
    ;
    const actual = try mantis.testing.typeInferVerbose(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = print, type = fn([]u8) void }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        foreign_import =
        \\            module = "console"
        \\            name = "log"
        \\            type = fn([]u8) void
        \\
        \\define =
        \\    name = symbol{ value = start, type = fn() void }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            return_type = void
        \\            body =
        \\                call =
        \\                    name = symbol{ value = print, type = fn([]u8) void }
        \\                    arguments =
        \\                        string{ value = "hello world", type = []u8 }
        \\                    type = void
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen import" {
    const allocator = std.testing.allocator;
    const source =
        \\print = foreign_import("console", "log", fn(x: i32) void)
        \\
        \\start = fn() void { print(42) }
    ;
    const actual = try mantis.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (import "console" "log" (func $print (param i32)))
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\    (global $arena (mut i32) (i32.const 0))
        \\
        \\    (func $start
        \\        (call $print
        \\            (i32.const 42)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize export" {
    const allocator = std.testing.allocator;
    const source =
        \\foreign_export("double", fn(x: i32) i32 {
        \\    x * 2
        \\})
    ;
    const actual = try mantis.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol foreign_export)
        \\(delimiter '(')
        \\(string "double")
        \\(delimiter ',')
        \\(keyword fn)
        \\(delimiter '(')
        \\(symbol x)
        \\(operator :)
        \\(symbol i32)
        \\(delimiter ')')
        \\(symbol i32)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol x)
        \\(operator *)
        \\(int 2)
        \\(new_line)
        \\(delimiter '}')
        \\(delimiter ')')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse export" {
    const allocator = std.testing.allocator;
    const source =
        \\foreign_export("double", fn(x: i32) i32 {
        \\    x * 2
        \\})
    ;
    const actual = try mantis.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(foreign_export "double" (fn [(x i32)] i32
        \\    (* x 2)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse named export" {
    const allocator = std.testing.allocator;
    const source =
        \\double = fn(x: i32) i32 {
        \\    x * 2
        \\}
        \\
        \\foreign_export("double", double)
    ;
    const actual = try mantis.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def double (fn [(x i32)] i32
        \\    (* x 2)))
        \\
        \\(foreign_export "double" double)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type check export" {
    const allocator = std.testing.allocator;
    const source =
        \\foreign_export("double", fn(x: i32) i32 {
        \\    x * 2
        \\})
    ;
    const actual = try mantis.testing.typeInfer(allocator, source, "\"double\"");
    defer allocator.free(actual);
    const expected =
        \\foreign_export =
        \\    name = "double"
        \\    value =
        \\        function =
        \\            parameters =
        \\                symbol{ value = x, type = i32 }
        \\            return_type = i32
        \\            body =
        \\                binary_op =
        \\                    kind = *
        \\                    left =
        \\                        symbol{ value = x, type = i32 }
        \\                    right =
        \\                        int{ value = 2, type = i32 }
        \\                    type = i32
        \\    type = void
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type check named export" {
    const allocator = std.testing.allocator;
    const source =
        \\double = fn(x: i32) i32 {
        \\    x * 2
        \\}
        \\
        \\foreign_export("double", double)
    ;
    const actual = try mantis.testing.typeInfer(allocator, source, "\"double\"");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = double, type = fn(i32) i32 }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            parameters =
        \\                symbol{ value = x, type = i32 }
        \\            return_type = i32
        \\            body =
        \\                binary_op =
        \\                    kind = *
        \\                    left =
        \\                        symbol{ value = x, type = i32 }
        \\                    right =
        \\                        int{ value = 2, type = i32 }
        \\                    type = i32
        \\
        \\foreign_export =
        \\    name = "double"
        \\    value =
        \\        symbol{ value = double, type = fn(i32) i32 }
        \\    type = void
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen foreign export" {
    const allocator = std.testing.allocator;
    const source =
        \\foreign_export("double", fn(x: i32) i32 {
        \\    x * 2
        \\})
    ;
    const actual = try mantis.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\    (global $arena (mut i32) (i32.const 0))
        \\
        \\    (func $double (param $x i32) (result i32)
        \\        (i32.mul
        \\            (local.get $x)
        \\            (i32.const 2)))
        \\
        \\    (export "double" (func $double)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen named foreign export" {
    const allocator = std.testing.allocator;
    const source =
        \\double = fn(x: i32) i32 {
        \\    x * 2
        \\}
        \\
        \\foreign_export("double", double)
    ;
    const actual = try mantis.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\    (global $arena (mut i32) (i32.const 0))
        \\
        \\    (func $double (param $x i32) (result i32)
        \\        (i32.mul
        \\            (local.get $x)
        \\            (i32.const 2)))
        \\
        \\    (export "double" (func $double)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen hello world" {
    const allocator = std.testing.allocator;
    const source =
        \\fd_write = foreign_import("wasi_unstable", "fd_write", fn(fd: i32, text: []u8, count: i32, out: i32) i32)
        \\
        \\stdout: i32 = 1
        \\
        \\start = fn() i32 {
        \\    text = "Hello, World!"
        \\    mut nwritten: i32 = undefined
        \\    fd_write(stdout, text, 1, 200)
        \\}
    ;
    const actual = try mantis.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (import "wasi_unstable" "fd_write" (func $fd_write (param i32) (param i32) (param i32) (param i32) (result i32)))
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\    (global $arena (mut i32) (i32.const 13))
        \\
        \\    (data (i32.const 0) "Hello, World!")
        \\
        \\    (global $stdout i32 (i32.const 1))
        \\
        \\    (func $start (result i32)
        \\        (local $text i32)
        \\        (local $0 i32)
        \\        (local $nwritten i32)
        \\        (local.set $text
        \\            (block (result i32)
        \\                (local.set $0
        \\                    (global.get $arena))
        \\                (i32.store
        \\                    (local.get $0)
        \\                    (i32.const 0))
        \\                (i32.store
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 4))
        \\                    (i32.const 13))
        \\                (global.set $arena
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 8)))
        \\                (local.get $0)))
        \\        (call $fd_write
        \\            (global.get $stdout)
        \\            (local.get $text)
        \\            (i32.const 1)
        \\            (i32.const 200)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
