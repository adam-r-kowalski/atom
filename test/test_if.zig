const std = @import("std");
const atom = @import("atom");

test "tokenize if then else" {
    const allocator = std.testing.allocator;
    const source =
        \\if x then y else z
    ;
    var intern = atom.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.tokenizer.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const actual = try atom.tokenizer.toString(allocator, intern, tokens);
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
    const reconstructed = try atom.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "parse if then else" {
    const allocator = std.testing.allocator;
    const source =
        \\if x then y else z
    ;
    var intern = atom.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.tokenizer.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
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
        \\    y
        \\else
        \\    z
    ;
    var intern = atom.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.tokenizer.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected =
        \\(if x y z)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse if multi line then else" {
    const allocator = std.testing.allocator;
    const source =
        \\if x then
        \\    a = y ^ 2
        \\    a * 5
        \\else
        \\    z
    ;
    var intern = atom.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.tokenizer.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected =
        \\(if x
        \\    (block
        \\        (def a (^ y 2))
        \\        (* a 5))
        \\    z)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse if then multi line else" {
    const allocator = std.testing.allocator;
    const source =
        \\if x then
        \\    y
        \\else
        \\    a = z ^ 2
        \\    a * 5
    ;
    var intern = atom.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.tokenizer.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected =
        \\(if x y
        \\    (block
        \\        (def a (^ z 2))
        \\        (* a 5)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse let on result of if then else" {
    const allocator = std.testing.allocator;
    const source =
        \\b = if x then
        \\        y
        \\    else
        \\        a = z ^ 2
        \\        a * 5
    ;
    var intern = atom.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.tokenizer.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected =
        \\(def b (if x y
        \\        (block
        \\            (def a (^ z 2))
        \\            (* a 5))))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse nested if then else" {
    const allocator = std.testing.allocator;
    const source =
        \\if x > y then 1
        \\else if x < y then -1
        \\else 0
    ;
    var intern = atom.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.tokenizer.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected =
        \\(if (> x y) 1 (if (< x y) -1 0))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
