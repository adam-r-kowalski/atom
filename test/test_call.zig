const std = @import("std");
const atom = @import("atom");

test "tokenize call" {
    const allocator = std.testing.allocator;
    const source = "f(x, y, z)";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const actual = try atom.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol f
        \\left paren
        \\symbol x
        \\comma
        \\symbol y
        \\comma
        \\symbol z
        \\right paren
    ;
    try std.testing.expectEqualStrings(expected, actual);
    const reconstructed = try atom.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "parse call" {
    const allocator = std.testing.allocator;
    const source = "f(x, y, z)";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(f x y z)";
    try std.testing.expectEqualStrings(expected, actual);
}
