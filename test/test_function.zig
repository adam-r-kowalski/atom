const std = @import("std");
const fusion = @import("fusion");

test "tokenize with no annotation" {
    const allocator = std.testing.allocator;
    const source = "double(x) = x + x";
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const actual = try fusion.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol double
        \\left paren
        \\symbol x
        \\right paren
        \\equal
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
    const source = "double(x) = x + x";
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try fusion.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try fusion.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(defn double [x] (+ x x))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize with annotation" {
    const allocator = std.testing.allocator;
    const source = "double(x: i32): i32 = x + x";
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const actual = try fusion.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol double
        \\left paren
        \\symbol x
        \\colon
        \\symbol i32
        \\right paren
        \\colon
        \\symbol i32
        \\equal
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
    const source = "double(x: i32): i32 = x + x";
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try fusion.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try fusion.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(defn double [(x i32)] i32 (+ x x))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse annotating multiple bindings" {
    const allocator = std.testing.allocator;
    const source = "add(x: i32, y: i32): i32 = x + y";
    var intern = fusion.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try fusion.tokenizer.Builtins.init(&intern);
    const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try fusion.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try fusion.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(defn add [(x i32) (y i32)] i32 (+ x y))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse annotating multiple bindings with no return type" {
    const allocator = std.testing.allocator;
    const source = "add(x: i32, y: i32) = x + y";
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
    const source = "add(x y): i32 = x + y";
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
        \\(defn add [x y] i32 (+ x y))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiple parameters annotating one parameter and return type" {
    const allocator = std.testing.allocator;
    const source = "add(x: i32, y): i32 = x + y";
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

test "parse multi line function" {
    const allocator = std.testing.allocator;
    const source =
        \\sum_squares(x: i32, y: i32): i32 =
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
