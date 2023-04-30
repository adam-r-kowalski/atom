const std = @import("std");
const atom = @import("atom");

test "tokenize single line define" {
    const allocator = std.testing.allocator;
    const source = "x = y + z";
    var intern = atom.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.tokenizer.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const actual = try atom.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol x
        \\equal
        \\symbol y
        \\plus
        \\symbol z
    ;
    try std.testing.expectEqualStrings(expected, actual);
    const reconstructed = try atom.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "parse single line define" {
    const allocator = std.testing.allocator;
    const source = "x = y + z";
    var intern = atom.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.tokenizer.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(def x (+ y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize annotated single line define" {
    const allocator = std.testing.allocator;
    const source = "x: i32 = y + z";
    var intern = atom.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.tokenizer.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const actual = try atom.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol x
        \\colon
        \\symbol i32
        \\equal
        \\symbol y
        \\plus
        \\symbol z
    ;
    try std.testing.expectEqualStrings(expected, actual);
    const reconstructed = try atom.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "parse single line define" {
    const allocator = std.testing.allocator;
    const source = "x: i32 = y + z";
    var intern = atom.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.tokenizer.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(def x i32 (+ y z))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize multi line define" {
    const allocator = std.testing.allocator;
    const source =
        \\x =
        \\  a = y + z
        \\  a - b
    ;
    var intern = atom.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.tokenizer.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const actual = try atom.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol x
        \\equal
        \\space 2
        \\symbol a
        \\equal
        \\symbol y
        \\plus
        \\symbol z
        \\space 2
        \\symbol a
        \\minus
        \\symbol b
    ;
    try std.testing.expectEqualStrings(expected, actual);
    const reconstructed = try atom.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "parse multi line define" {
    const allocator = std.testing.allocator;
    const source =
        \\x =
        \\  a = y + z
        \\  a - b
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
        \\(def x
        \\    (block
        \\        (def a (+ y z))
        \\        (- a b)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi line define with type annotation" {
    const allocator = std.testing.allocator;
    const source =
        \\x: i32 =
        \\  a: i32 = y + z
        \\  a - b
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
        \\(def x i32
        \\    (block
        \\        (def a i32 (+ y z))
        \\        (- a b)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}
