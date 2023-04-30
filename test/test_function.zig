const std = @import("std");
const fusion = @import("fusion");

test "tokenize with no annotation" {
    const allocator = std.testing.allocator;
    const source =
        \\double = \x. x + x
    ;
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
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
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
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
        \\double: i32 -> i32 = \x. x + x
    ;
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const actual = try fusion.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol double
        \\colon
        \\symbol i32
        \\arrow
        \\symbol i32
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
        \\double: i32 -> i32 = \x. x + x
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
        \\(def double (-> i32 i32) (fn [x] (+ x x)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize annotating bindings" {
    const allocator = std.testing.allocator;
    const source =
        \\double = \(x: i32) -> i32. x + x
    ;
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
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
        \\symbol i32
        \\right paren
        \\arrow
        \\symbol i32
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
        \\double = \(x: i32) -> i32. x + x
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
        \\(def double (fn [(x i32)] i32 (+ x x)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse annotating multiple bindings" {
    const allocator = std.testing.allocator;
    const source =
        \\add = \(x: i32) (y: i32) -> i32. x + y
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
        \\(def add (fn [(x i32) (y i32)] i32 (+ x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse annotating multiple bindings with no return type" {
    const allocator = std.testing.allocator;
    const source =
        \\add = \(x: i32) (y: i32). x + y
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
        \\(def add (fn [(x i32) (y i32)] (+ x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiple parameters annotating only return type" {
    const allocator = std.testing.allocator;
    const source =
        \\add = \x y -> i32. x + y
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
        \\(def add (fn [x y] i32 (+ x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiple parameters annotating one parameter and return type" {
    const allocator = std.testing.allocator;
    const source =
        \\add = \(x: i32) y -> i32. x + y
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
        \\(def add (fn [(x i32) y] i32 (+ x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi parameter annotation" {
    const allocator = std.testing.allocator;
    const source =
        \\add: i32 -> i32 -> i32 = \x y. x + y
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
        \\(def add (-> i32 (-> i32 i32)) (fn [x y] (+ x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse both kinds of annotations" {
    const allocator = std.testing.allocator;
    const source =
        \\add: i32 -> i32 -> Bool = \(x: i32) (y: i32) -> Bool. x > y
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
        \\(def add (-> i32 (-> i32 Bool)) (fn [(x i32) (y i32)] Bool (> x y)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi line function" {
    const allocator = std.testing.allocator;
    const source =
        \\sum_squares = \(x: i32) (y: i32) -> i32.
        \\    x_squared = x ^ 2
        \\    y_squared = y ^ 2
        \\    x_squared + y_squared
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
        \\(def sum_squares (fn [(x i32) (y i32)] i32
        \\    (block
        \\        (def x_squared (^ x 2))
        \\        (def y_squared (^ y 2))
        \\        (+ x_squared y_squared))))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
