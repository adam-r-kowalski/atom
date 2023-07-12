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
        \\    name = symbol{ value = start, type = () vec[i32] }
        \\    type = void
        \\    mutable = false
        \\    value =
        \\        function =
        \\            return_type = vec[i32]
        \\            body =
        \\                array =
        \\                    expressions =
        \\                        int{ value = 1, type = i32 }
        \\                        int{ value = 2, type = i32 }
        \\                        int{ value = 3, type = i32 }
        \\                    type = vec[i32]
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
