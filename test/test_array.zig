const std = @import("std");
const mantis = @import("mantis");

test "tokenize array" {
    const allocator = std.testing.allocator;
    const source =
        \\start = () vec[i32] {
        \\    [1, 2, 3]
        \\}
    ;
    const actual = try mantis.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol start)
        \\(operator =)
        \\(delimiter '(')
        \\(delimiter ')')
        \\(symbol vec)
        \\(delimiter '[')
        \\(symbol i32)
        \\(delimiter ']')
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
        \\start = () vec[i32] {
        \\    [1, 2, 3]
        \\}
    ;
    const actual = try mantis.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def start (fn [] (index vec i32)
        \\    [
        \\        1
        \\        2
        \\        3]))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer array" {
    const allocator = std.testing.allocator;
    const source =
        \\start = () vec[i32] {
        \\    [1, 2, 3]
        \\}
    ;
    const actual = try mantis.testing.typeInfer(allocator, source, "start");
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

// test "codegen struct" {
//     const allocator = std.testing.allocator;
//     const source =
//         \\Person = struct {
//         \\    name: str,
//         \\    age: u8,
//         \\}
//         \\
//         \\start = () Person {
//         \\    {
//         \\        name: "Bob",
//         \\        age: 42,
//         \\    }
//         \\}
//     ;
//     const actual = try mantis.testing.codegen(allocator, source);
//     defer allocator.free(actual);
//     const expected =
//         \\(module
//         \\
//         \\    (memory 1)
//         \\    (export "memory" (memory 0))
//         \\
//         \\    (data (i32.const 0) "Bob")
//         \\
//         \\    (global $core/arena (mut i32) (i32.const 3))
//         \\
//         \\    (func $core/alloc (param $size i32) (result i32)
//         \\        (local $ptr i32)
//         \\        (local.tee $ptr
//         \\            (global.get $core/arena))
//         \\        (global.set $core/arena
//         \\            (i32.add
//         \\                (local.get $ptr)
//         \\                (local.get $size))))
//         \\
//         \\    (func $start (result i32)
//         \\        (local $0 i32)
//         \\        (local $1 i32)
//         \\        (local.set $0
//         \\            (call $core/alloc
//         \\                (i32.const 12)))
//         \\        (local.set $1
//         \\            (call $core/alloc
//         \\                (i32.const 8)))
//         \\        (block (result i32)
//         \\            (memory.copy
//         \\                (local.get $0)
//         \\                (block (result i32)
//         \\                    (i32.store
//         \\                        (local.get $1)
//         \\                        (i32.const 0))
//         \\                    (i32.store
//         \\                        (i32.add
//         \\                            (local.get $1)
//         \\                            (i32.const 4))
//         \\                        (i32.const 3))
//         \\                    (local.get $1))
//         \\                (i32.const 8))
//         \\            (i32.store8
//         \\                (i32.add
//         \\                    (local.get $0)
//         \\                    (i32.const 8))
//         \\                (i32.const 42))
//         \\            (local.get $0)))
//         \\
//         \\    (export "_start" (func $start)))
//     ;
//     try std.testing.expectEqualStrings(expected, actual);
// }
