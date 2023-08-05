const std = @import("std");
const goat = @import("goat");

test "tokenize struct" {
    const allocator = std.testing.allocator;
    const source =
        \\Person = struct {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\start = () Person {
        \\    {
        \\        name: "Bob",
        \\        age: 42,
        \\    }
        \\}
    ;
    const actual = try goat.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol Person)
        \\(operator =)
        \\(keyword struct)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol name)
        \\(operator :)
        \\(symbol str)
        \\(delimiter ',')
        \\(new_line)
        \\(symbol age)
        \\(operator :)
        \\(symbol u8)
        \\(delimiter ',')
        \\(new_line)
        \\(delimiter '}')
        \\(new_line)
        \\(symbol start)
        \\(operator =)
        \\(delimiter '(')
        \\(delimiter ')')
        \\(symbol Person)
        \\(delimiter '{')
        \\(new_line)
        \\(delimiter '{')
        \\(new_line)
        \\(symbol name)
        \\(operator :)
        \\(string "Bob")
        \\(delimiter ',')
        \\(new_line)
        \\(symbol age)
        \\(operator :)
        \\(int 42)
        \\(delimiter ',')
        \\(new_line)
        \\(delimiter '}')
        \\(new_line)
        \\(delimiter '}')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse struct" {
    const allocator = std.testing.allocator;
    const source =
        \\Person = struct {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\start = () Person {
        \\    {
        \\        name: "Bob",
        \\        age: 42,
        \\    }
        \\}
    ;
    const actual = try goat.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def Person (struct
        \\    name str
        \\    age u8))
        \\
        \\(def start (fn [] Person
        \\    (struct_literal
        \\        name "Bob"
        \\        age 42)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer struct" {
    const allocator = std.testing.allocator;
    const source =
        \\Person = struct {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\start = () Person {
        \\    {
        \\        name: "Bob",
        \\        age: 42,
        \\    }
        \\}
    ;
    const actual = try goat.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = start, type = () struct{ name: str, age: u8 } }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            return_type = struct{ name: str, age: u8 }
        \\            body =
        \\                struct_literal =
        \\                    type = struct_literal{ name: str, age: u8 } as struct{ name: str, age: u8 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen struct" {
    const allocator = std.testing.allocator;
    const source =
        \\Person = struct {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\start = () Person {
        \\    {
        \\        name: "Bob",
        \\        age: 42,
        \\    }
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
        \\    (data (i32.const 0) "Bob")
        \\
        \\    (global $core/arena (mut i32) (i32.const 3))
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
        \\            (memory.copy
        \\                (local.get $0)
        \\                (block (result i32)
        \\                    (i32.store
        \\                        (local.get $1)
        \\                        (i32.const 0))
        \\                    (i32.store
        \\                        (i32.add
        \\                            (local.get $1)
        \\                            (i32.const 4))
        \\                        (i32.const 3))
        \\                    (local.get $1))
        \\                (i32.const 8))
        \\            (i32.store8
        \\                (i32.add
        \\                    (local.get $0)
        \\                    (i32.const 8))
        \\                (i32.const 42))
        \\            (local.get $0)))
        \\
        \\    (export "_start" (func $start)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
