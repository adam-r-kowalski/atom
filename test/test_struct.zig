const std = @import("std");
const mantis = @import("mantis");

test "tokenize struct" {
    const allocator = std.testing.allocator;
    const source =
        \\Person = struct {
        \\    name: str,
        \\    age: u8,
        \\}
        \\
        \\start = fn() Person {
        \\    {
        \\        name: "Bob",
        \\        age: 42,
        \\    }
        \\}
    ;
    const actual = try mantis.testing.tokenize(allocator, source);
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
        \\(keyword fn)
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
        \\start = fn() Person {
        \\    {
        \\        name: "Bob",
        \\        age: 42,
        \\    }
        \\}
    ;
    const actual = try mantis.testing.parse(allocator, source);
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
        \\start = fn() Person {
        \\    {
        \\        name: "Bob",
        \\        age: 42,
        \\    }
        \\}
    ;
    const actual = try mantis.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = start, type = fn() struct{ name: str, age: u8 } }
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
        \\start = fn() Person {
        \\    {
        \\        name: "Bob",
        \\        age: 42,
        \\    }
        \\}
    ;
    const actual = try mantis.testing.codegen(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = start, type = fn() struct{ name: str, age: u8 } }
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
