const std = @import("std");
const mantis = @import("mantis");

test "tokenize mutable binding" {
    const allocator = std.testing.allocator;
    const source =
        \\start = fn() i32 {
        \\    mut x: i32 = 0
        \\    x += 1
        \\    x
        \\}
    ;
    const actual = try mantis.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol start)
        \\(operator =)
        \\(keyword fn)
        \\(delimiter '(')
        \\(delimiter ')')
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
        \\start = fn() i32 {
        \\    mut x: i32 = 0
        \\    x += 1
        \\    x
        \\}
    ;
    const actual = try mantis.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def start (fn [] i32
        \\    (block
        \\        (def mut x i32 0)
        \\        (+= x 1)
        \\        x)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer mutable binding" {
    const allocator = std.testing.allocator;
    const source =
        \\start = fn() i32 {
        \\    mut x: i32 = 0
        \\    x += 1
        \\    x
        \\}
    ;
    const actual = try mantis.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = start, type = fn() i32 }
        \\    type = void
        \\    mutable = false
        \\    value = 
        \\        function =
        \\            return_type = i32
        \\            body = 
        \\                define =
        \\                    name = symbol{ value = x, type = i32 }
        \\                    type = void
        \\                    mutable = true
        \\                    value = int{ value = 0, type = i32 }
        \\                add_assign =
        \\                    name = symbol{ value = x, type = i32 }
        \\                    type = void
        \\                    value = int{ value = 1, type = i32 }
        \\                symbol{ value = x, type = i32 }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "codegen mutable binding" {
    const allocator = std.testing.allocator;
    const source =
        \\start = fn() i32 {
        \\    mut x: i32 = 0
        \\    x += 1
        \\    x
        \\}
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
