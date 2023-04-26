const std = @import("std");
const fusion = @import("fusion");

test "tokenize with no annotation" {
    const allocator = std.testing.allocator;
    const source =
        \\double = \x. x + x
    ;
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const actual = try fusion.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol double
        \\equal
        \\backslash
        \\symbol x
        \\dot
        \\symbol x
        \\plus
        \\symbol x
    ;
    try std.testing.expectEqualStrings(expected, actual);
    const reconstructed = try fusion.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "tokenize with annotation" {
    const allocator = std.testing.allocator;
    const source =
        \\double: I32 -> I32 = \x. x + x
    ;
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const actual = try fusion.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol double
        \\colon
        \\symbol I32
        \\arrow
        \\symbol I32
        \\equal
        \\backslash
        \\symbol x
        \\dot
        \\symbol x
        \\plus
        \\symbol x
    ;
    try std.testing.expectEqualStrings(expected, actual);
    const reconstructed = try fusion.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "tokenize annotating bindings" {
    const allocator = std.testing.allocator;
    const source =
        \\double = \(x: I32) -> I32. x + x
    ;
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const actual = try fusion.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol double
        \\equal
        \\backslash
        \\left paren
        \\symbol x
        \\colon
        \\symbol I32
        \\right paren
        \\arrow
        \\symbol I32
        \\dot
        \\symbol x
        \\plus
        \\symbol x
    ;
    try std.testing.expectEqualStrings(expected, actual);
    const reconstructed = try fusion.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "parse with no annotation" {
    const allocator = std.testing.allocator;
    const source =
        \\double = \x. x + x
    ;
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const ast = try fusion.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try fusion.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected =
        \\(def double (fn (x) (+ x x)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
