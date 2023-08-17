const std = @import("std");
const wave = @import("wave");

test "tokenize import" {
    const allocator = std.testing.allocator;
    const source =
        \\@import("console", "log")
        \\fn log(msg: str) -> void
    ;
    const actual = try wave.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(attribute @import)
        \\(delimiter '(')
        \\(string "console")
        \\(delimiter ',')
        \\(string "log")
        \\(delimiter ')')
        \\(new_line)
        \\(keyword fn)
        \\(symbol log)
        \\(delimiter '(')
        \\(symbol msg)
        \\(operator :)
        \\(symbol str)
        \\(delimiter ')')
        \\(operator ->)
        \\(symbol void)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse import" {
    const allocator = std.testing.allocator;
    const source =
        \\@import("console", "log")
        \\fn log(msg: str) -> void
    ;
    const actual = try wave.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(@import "console" "log"
        \\    (fn log [(msg str)] void))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse import with no arguments" {
    const allocator = std.testing.allocator;
    const source =
        \\@import
        \\fn log(msg: str) -> void
    ;
    const actual = try wave.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(@import
        \\    (fn log [(msg str)] void))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer import" {
    const allocator = std.testing.allocator;
    const source =
        \\@import("console", "log")
        \\fn print(msg: str) -> void
        \\
        \\fn start() -> void {
        \\    print("hello world")
        \\}
    ;
    const actual = try wave.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\foreign_import =
        \\    module = "console"
        \\    name = "log"
        \\    prototype =
        \\        name = symbol{ value = print, type = fn(msg: str) -> void }
        \\        parameters =
        \\            symbol{ value = msg, type = str }
        \\        return_type = void
        \\    type = fn(msg: str) -> void
        \\
        \\function =
        \\    name = symbol{ value = start, type = fn() -> void }
        \\    return_type = void
        \\    body =
        \\        call =
        \\            function = symbol{ value = print, type = fn(msg: str) -> void }
        \\            arguments =
        \\                argument =
        \\                    mutable = false
        \\                    value = string{ value = "hello world", type = str }
        \\            type = void
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer import with no arguments" {
    const allocator = std.testing.allocator;
    const source =
        \\@import
        \\fn print(msg: str) -> void
        \\
        \\fn start() -> void {
        \\    print("hello world")
        \\}
    ;
    const actual = try wave.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\foreign_import =
        \\    module = "host"
        \\    name = "print"
        \\    prototype =
        \\        name = symbol{ value = print, type = fn(msg: str) -> void }
        \\        parameters =
        \\            symbol{ value = msg, type = str }
        \\        return_type = void
        \\    type = fn(msg: str) -> void
        \\
        \\function =
        \\    name = symbol{ value = start, type = fn() -> void }
        \\    return_type = void
        \\    body =
        \\        call =
        \\            function = symbol{ value = print, type = fn(msg: str) -> void }
        \\            arguments =
        \\                argument =
        \\                    mutable = false
        \\                    value = string{ value = "hello world", type = str }
        \\            type = void
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen import" {
    const allocator = std.testing.allocator;
    const source =
        \\@import("console", "log")
        \\fn print(msg: i32) -> void
        \\
        \\fn start() -> void {
        \\    print(42)
        \\}
    ;
    const actual = try wave.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (import "console" "log" (func $print (param i32)))
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (func $start
        \\        (call $print
        \\            (i32.const 42)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen import with no arguments" {
    const allocator = std.testing.allocator;
    const source =
        \\@import
        \\fn print(msg: i32) -> void
        \\
        \\fn start() -> void {
        \\    print(42)
        \\}
    ;
    const actual = try wave.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (import "host" "print" (func $print (param i32)))
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
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
        \\@export("double")
        \\fn double(x: i32) -> i32 {
        \\    x * 2
        \\}
    ;
    const actual = try wave.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(attribute @export)
        \\(delimiter '(')
        \\(string "double")
        \\(delimiter ')')
        \\(new_line)
        \\(keyword fn)
        \\(symbol double)
        \\(delimiter '(')
        \\(symbol x)
        \\(operator :)
        \\(symbol i32)
        \\(delimiter ')')
        \\(operator ->)
        \\(symbol i32)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol x)
        \\(operator *)
        \\(int 2)
        \\(new_line)
        \\(delimiter '}')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse export" {
    const allocator = std.testing.allocator;
    const source =
        \\@export("double")
        \\fn double(x: i32) -> i32 {
        \\    x * 2
        \\}
    ;
    const actual = try wave.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(@export "double"
        \\    (fn double [(x i32)] i32
        \\        (* x 2)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse export with no arguments" {
    const allocator = std.testing.allocator;
    const source =
        \\@export
        \\fn double(x: i32) -> i32 {
        \\    x * 2
        \\}
    ;
    const actual = try wave.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(@export
        \\    (fn double [(x i32)] i32
        \\        (* x 2)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer export" {
    const allocator = std.testing.allocator;
    const source =
        \\@export("double")
        \\fn double(x: i32) -> i32 {
        \\    x * 2
        \\}
    ;
    const actual = try wave.testing.typeInfer(allocator, source, "double");
    defer allocator.free(actual);
    const expected =
        \\foreign_export =
        \\    name = "double"
        \\    function =
        \\        name = symbol{ value = double, type = fn(x: i32) -> i32 }
        \\        parameters =
        \\            symbol{ value = x, type = i32 }
        \\        return_type = i32
        \\        body =
        \\            binary_op =
        \\                kind = *
        \\                left =
        \\                    symbol{ value = x, type = i32 }
        \\                right =
        \\                    int{ value = 2, type = i32 }
        \\                type = i32
        \\    type = fn(x: i32) -> i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer export with no argument" {
    const allocator = std.testing.allocator;
    const source =
        \\@export
        \\fn double(x: i32) -> i32 {
        \\    x * 2
        \\}
    ;
    const actual = try wave.testing.typeInfer(allocator, source, "double");
    defer allocator.free(actual);
    const expected =
        \\foreign_export =
        \\    name = "double"
        \\    function =
        \\        name = symbol{ value = double, type = fn(x: i32) -> i32 }
        \\        parameters =
        \\            symbol{ value = x, type = i32 }
        \\        return_type = i32
        \\        body =
        \\            binary_op =
        \\                kind = *
        \\                left =
        \\                    symbol{ value = x, type = i32 }
        \\                right =
        \\                    int{ value = 2, type = i32 }
        \\                type = i32
        \\    type = fn(x: i32) -> i32
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen foreign export" {
    const allocator = std.testing.allocator;
    const source =
        \\@export("double")
        \\fn double(x: i32) -> i32 {
        \\    x * 2
        \\}
    ;
    const actual = try wave.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
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

test "codegen foreign export with no arguments" {
    const allocator = std.testing.allocator;
    const source =
        \\@export
        \\fn double(x: i32) -> i32 {
        \\    x * 2
        \\}
    ;
    const actual = try wave.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
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
        \\@import("wasi_unstable", "fd_write")
        \\fn fd_write(fd: i32, text: str, count: i32, out: i32) -> i32
        \\
        \\stdout: i32 = 1
        \\
        \\fn start() -> i32 {
        \\    text = "Hello, World!"
        \\    mut nwritten: i32 = undefined
        \\    fd_write(stdout, text, 1, 200)
        \\}
    ;
    const actual = try wave.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (import "wasi_unstable" "fd_write" (func $fd_write (param i32) (param i32) (param i32) (param i32) (result i32)))
        \\
        \\    (memory 1)
        \\    (export "memory" (memory 0))
        \\
        \\    (data (i32.const 0) "Hello, World!")
        \\
        \\    (global $core/arena (mut i32) (i32.const 13))
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
        \\    (global $stdout i32 (i32.const 1))
        \\
        \\    (func $start (result i32)
        \\        (local $text i32)
        \\        (local $nwritten i32)
        \\        (local $0 i32)
        \\        (local.set $0
        \\            (call $core/alloc
        \\                (i32.const 8)))
        \\        (local.set $text
        \\            (block (result i32)
        \\                (i32.store
        \\                    (local.get $0)
        \\                    (i32.const 0))
        \\                (i32.store
        \\                    (i32.add
        \\                        (local.get $0)
        \\                        (i32.const 4))
        \\                    (i32.const 13))
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

test "codegen echo" {
    const allocator = std.testing.allocator;
    const source =
        \\@import("wasi_unstable", "fd_read")
        \\fn fd_read(fd: i32, mut iovs: str, iov_count: i32, mut nread: i32) -> i32
        \\
        \\@import("wasi_unstable", "fd_write")
        \\fn fd_write(fd: i32, iovs: str, iov_count: i32, mut nwritten: i32) -> i32
        \\
        \\stdin: i32 = 0
        \\stdout: i32 = 1
        \\
        \\fn start() -> void {
        \\    mut text = empty(u8, 100)
        \\    mut nread = undefined
        \\    mut nwritten = undefined
        \\    fd_read(stdin, mut text, 1, mut nread)
        \\    _ = fd_write(stdout, text, 1, mut nwritten)
        \\}
    ;
    const actual = try wave.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (import "wasi_unstable" "fd_read" (func $fd_read (param i32) (param i32) (param i32) (param i32) (result i32)))
        \\    (import "wasi_unstable" "fd_write" (func $fd_write (param i32) (param i32) (param i32) (param i32) (result i32)))
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
        \\    (func $core/empty (param $size i32) (param $len i32) (result i32)
        \\        (local $ptr i32)
        \\            (local.set $ptr
        \\                (call $core/alloc
        \\                (i32.const 8)))
        \\        (i32.store
        \\            (local.get $ptr)
        \\            (call $core/alloc
        \\            (i32.mul
        \\                (local.get $size)
        \\                (local.get $len))))
        \\        (i32.store
        \\            (i32.add
        \\                (local.get $ptr)
        \\                (i32.const 4))
        \\            (local.get $len))
        \\        (local.get $ptr))
        \\
        \\    (global $stdin i32 (i32.const 0))
        \\    (global $stdout i32 (i32.const 1))
        \\
        \\    (func $start
        \\        (local $text i32)
        \\        (local $nread i32)
        \\        (local $nwritten i32)
        \\        (local $nread/ptr i32)
        \\        (local $nwritten/ptr i32)
        \\        (local.set $nread/ptr
        \\            (call $core/alloc
        \\                (i32.const 4)))
        \\        (local.set $nwritten/ptr
        \\            (call $core/alloc
        \\                (i32.const 4)))
        \\        (local.set $text
        \\            (call $core/empty
        \\                (i32.const 1)
        \\                (i32.const 100)))
        \\        (drop
        \\            (i32.store
        \\                (local.get $nread/ptr)
        \\                (local.get $nread))
        \\            (call $fd_read
        \\                (global.get $stdin)
        \\                (local.get $text)
        \\                (i32.const 1)
        \\                (local.get $nread/ptr))
        \\            (local.set $nread
        \\                (i32.load
        \\                    (local.get $nread/ptr))))
        \\        (drop
        \\            (i32.store
        \\                (local.get $nwritten/ptr)
        \\                (local.get $nwritten))
        \\            (call $fd_write
        \\                (global.get $stdout)
        \\                (local.get $text)
        \\                (i32.const 1)
        \\                (local.get $nwritten/ptr))
        \\            (local.set $nwritten
        \\                (i32.load
        \\                    (local.get $nwritten/ptr)))))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
