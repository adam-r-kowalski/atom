const std = @import("std");
const neuron = @import("neuron");

test "tokenize import" {
    const allocator = std.testing.allocator;
    const source =
        \\print = foreign_import("console", "log", fn(msg: str) void)
    ;
    const actual = try neuron.testing.tokenize(allocator, source);
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
        \\(symbol str)
        \\(delimiter ')')
        \\(symbol void)
        \\(delimiter ')')
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse import" {
    const allocator = std.testing.allocator;
    const source =
        \\print = foreign_import("console", "log", fn(msg: str) void)
    ;
    const actual = try neuron.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def print (foreign_import "console" "log" (fn [(msg str)] void)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type check import" {
    const allocator = std.testing.allocator;
    const source =
        \\print = foreign_import("console", "log", fn(msg: str) void)
        \\
        \\start = fn() void {
        \\    print("hello world")
        \\}
    ;
    const actual = try neuron.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = print, type = fn(str) void }
        \\    type = void
        \\    value = 
        \\        foreign_import =
        \\            module = "console"
        \\            name = "log"
        \\            type = fn(str) void
        \\
        \\define =
        \\    name = symbol{ value = start, type = fn() void }
        \\    type = void
        \\    value = 
        \\        function =
        \\            return_type = void
        \\            body = 
        \\                call =
        \\                    symbol{ value = print, type = fn(str) void }
        \\                    arguments =
        \\                        string{ value = "hello world", type = str }
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
    const actual = try neuron.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
        \\
        \\    (import "console" "log" (func $print (param i32)))
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
    const actual = try neuron.testing.tokenize(allocator, source);
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
    const actual = try neuron.testing.parse(allocator, source);
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
    const actual = try neuron.testing.parse(allocator, source);
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
    const actual = try neuron.testing.typeInfer(allocator, source, "\"double\"");
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
        \\                    left = symbol{ value = x, type = i32 }
        \\                    right = int{ value = 2, type = i32 }
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
    const actual = try neuron.testing.typeInfer(allocator, source, "\"double\"");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = double, type = fn(i32) i32 }
        \\    type = void
        \\    value = 
        \\        function =
        \\            parameters =
        \\                symbol{ value = x, type = i32 }
        \\            return_type = i32
        \\            body = 
        \\                binary_op =
        \\                    kind = *
        \\                    left = symbol{ value = x, type = i32 }
        \\                    right = int{ value = 2, type = i32 }
        \\                    type = i32
        \\
        \\foreign_export =
        \\    name = "double"
        \\    value = symbol{ value = double, type = fn(i32) i32 }
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
    const actual = try neuron.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
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
    const actual = try neuron.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(module
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
