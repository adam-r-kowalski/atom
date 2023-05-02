const std = @import("std");
const atom = @import("atom");

test "tokenize add then multiply" {
    const allocator = std.testing.allocator;
    const source = "x + y * z";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const actual = try atom.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol x
        \\plus
        \\symbol y
        \\times
        \\symbol z
    ;
    try std.testing.expectEqualStrings(expected, actual);
    const reconstructed = try atom.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "parse add then multiply" {
    const allocator = std.testing.allocator;
    const source = "x + y * z";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(+ x (* y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiply then add" {
    const allocator = std.testing.allocator;
    const source = "x * y + z";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(+ (* x y) z)";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiply then grouped add" {
    const allocator = std.testing.allocator;
    const source = "x * (y + z)";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(* x (+ y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiply is left associative" {
    const allocator = std.testing.allocator;
    const source = "x * y * z";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(* (* x y) z)";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse exponentiate is right associative" {
    const allocator = std.testing.allocator;
    const source = "x ^ y ^ z";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(^ x (^ y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse greater has lower precedence then add" {
    const allocator = std.testing.allocator;
    const source = "a + b > c + d";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(> (+ a b) (+ c d))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse grouped greater" {
    const allocator = std.testing.allocator;
    const source = "a + (b > c) + d";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(+ a (+ (> b c) d))";
    try std.testing.expectEqualStrings(expected, actual);
}
