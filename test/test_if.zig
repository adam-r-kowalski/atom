const std = @import("std");
const fusion = @import("fusion");

test "tokenize if then else" {
    const allocator = std.testing.allocator;
    const source =
        \\if x then y else z
    ;
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const actual = try fusion.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\if
        \\symbol x
        \\then
        \\symbol y
        \\else
        \\symbol z
    ;
    try std.testing.expectEqualStrings(expected, actual);
    const reconstructed = try fusion.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "parse if then else" {
    const allocator = std.testing.allocator;
    const source =
        \\if x then y else z
    ;
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try fusion.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try fusion.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected =
        \\(if x y z)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse if then else across multiple lines" {
    const allocator = std.testing.allocator;
    const source =
        \\if x then
        \\  y
        \\else
        \\  z
    ;
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try fusion.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try fusion.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected =
        \\(if x y z)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
