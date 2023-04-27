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
        \\(def double (fn [x] (+ x x)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
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

test "parse with annotation" {
    const allocator = std.testing.allocator;
    const source =
        \\double: I32 -> I32 = \x. x + x
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
        \\(def double (-> I32 I32) (fn [x] (+ x x)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
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

test "parse annotating bindings" {
    const allocator = std.testing.allocator;
    const source =
        \\double = \(x: I32) -> I32. x + x
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
        \\(def double (fn [(x I32)] I32 (+ x x)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse annotating multiple bindings" {
    const allocator = std.testing.allocator;
    const source =
        \\add = \(x: I32) (y: I32) -> I32. x + y
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
        \\(def add (fn [(x I32) (y I32)] I32 (+ x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse annotating multiple bindings with no return type" {
    const allocator = std.testing.allocator;
    const source =
        \\add = \(x: I32) (y: I32). x + y
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
        \\(def add (fn [(x I32) (y I32)] (+ x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiple parameters annotating only return type" {
    const allocator = std.testing.allocator;
    const source =
        \\add = \x y -> I32. x + y
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
        \\(def add (fn [x y] I32 (+ x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiple parameters annotating one parameter and return type" {
    const allocator = std.testing.allocator;
    const source =
        \\add = \(x: I32) y -> I32. x + y
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
        \\(def add (fn [(x I32) y] I32 (+ x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi parameter annotation" {
    const allocator = std.testing.allocator;
    const source =
        \\add: I32 -> I32 -> I32 = \x y. x + y
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
        \\(def add (-> I32 (-> I32 I32)) (fn [x y] (+ x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse both kinds of annotations" {
    const allocator = std.testing.allocator;
    const source =
        \\add: I32 -> I32 -> Bool = \(x: I32) (y: I32) -> Bool. x > y
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
        \\(def add (-> I32 (-> I32 Bool)) (fn [(x I32) (y I32)] Bool (> x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
