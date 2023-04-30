const std = @import("std");
const fusion = @import("fusion");

test "tokenize add then multiply" {
    const allocator = std.testing.allocator;
    const source =
        \\x + y * z
    ;
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const actual = try fusion.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol x
        \\plus
        \\symbol y
        \\times
        \\symbol z
    ;
    try std.testing.expectEqualStrings(expected, actual);
    const reconstructed = try fusion.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "parse add then multiply" {
    const allocator = std.testing.allocator;
    const source =
        \\x + y * z
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
        \\(+ x (* y z))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiply then add" {
    const allocator = std.testing.allocator;
    const source =
        \\x * y + z
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
        \\(+ (* x y) z)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiply then grouped add" {
    const allocator = std.testing.allocator;
    const source =
        \\x * (y + z)
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
        \\(* x (+ y z))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiply is left associative" {
    const allocator = std.testing.allocator;
    const source =
        \\x * y * z
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
        \\(* (* x y) z)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse exponentiate is right associative" {
    const allocator = std.testing.allocator;
    const source =
        \\x ^ y ^ z
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
        \\(^ x (^ y z))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse arrow is right associative" {
    const allocator = std.testing.allocator;
    const source =
        \\x -> y -> z
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
        \\(-> x (-> y z))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse grouped arrow" {
    const allocator = std.testing.allocator;
    const source =
        \\(x -> y) -> z
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
        \\(-> (-> x y) z)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse greater has lower precedence then add" {
    const allocator = std.testing.allocator;
    const source =
        \\a + b > c + d
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
        \\(> (+ a b) (+ c d))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse grouped greater" {
    const allocator = std.testing.allocator;
    const source =
        \\a + (b > c) + d
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
        \\(+ a (+ (> b c) d))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
