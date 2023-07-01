const std = @import("std");
const mantis = @import("mantis");

test "tokenize enum" {
    const allocator = std.testing.allocator;
    const source =
        \\Grade = enum {
        \\    a,
        \\    b,
        \\    c,
        \\    d,
        \\    f,
        \\}
        \\
        \\start = fn() Grade {
        \\    Grade.a
        \\}
    ;
    const actual = try mantis.testing.tokenize(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(symbol Grade)
        \\(operator =)
        \\(keyword enum)
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
        \\(symbol start)
        \\(operator =)
        \\(keyword fn)
        \\(delimiter '(')
        \\(delimiter ')')
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
        \\Grade = enum {
        \\    a,
        \\    b,
        \\    c,
        \\    d,
        \\    f,
        \\}
        \\
        \\start = fn() Grade {
        \\    Grade.a
        \\}
    ;
    const actual = try mantis.testing.parse(allocator, source);
    defer allocator.free(actual);
    const expected =
        \\(def Grade (enum
        \\    a
        \\    b
        \\    c
        \\    d
        \\    f))
        \\
        \\(def start (fn [] Grade
        \\    (. Grade a)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer enum" {
    const allocator = std.testing.allocator;
    const source =
        \\Grade = enum {
        \\    a,
        \\    b,
        \\    c,
        \\    d,
        \\    f,
        \\}
        \\
        \\start = fn() Grade {
        \\    Grade.a
        \\}
    ;
    const actual = try mantis.testing.typeInfer(allocator, source, "start");
    defer allocator.free(actual);
    const expected =
        \\define =
        \\    name = symbol{ value = start, type = fn() enum{ a, b, c, d, f } }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            return_type = enum{ a, b, c, d, f }
        \\            body =
        \\                variant =
        \\                    value = a
        \\                    type = enum{ a, b, c, d, f }
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
