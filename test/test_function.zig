const std = @import("std");
const atom = @import("atom");

test "tokenize with no annotation" {
    const allocator = std.testing.allocator;
    const source = "double(x) = x + x";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const actual = try atom.tokenizer.toString(allocator, intern, tokens);
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
    const reconstructed = try atom.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "parse with no annotation" {
    const allocator = std.testing.allocator;
    const source = "double(x) = x + x";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(defn double [x] (+ x x))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "tokenize with annotation" {
    const allocator = std.testing.allocator;
    const source = "double(x: i32) -> i32 = x + x";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const actual = try atom.tokenizer.toString(allocator, intern, tokens);
    defer allocator.free(actual);
    const expected =
        \\symbol double
        \\left paren
        \\symbol x
        \\colon
        \\symbol i32
        \\right paren
        \\arrow
        \\symbol i32
        \\equal
        \\symbol x
        \\plus
        \\symbol x
    ;
    try std.testing.expectEqualStrings(expected, actual);
    const reconstructed = try atom.tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
}

test "parse with annotation" {
    const allocator = std.testing.allocator;
    const source = "double(x: i32) -> i32 = x + x";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(defn double [(x i32)] i32 (+ x x))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse annotating multiple bindings" {
    const allocator = std.testing.allocator;
    const source = "add(x: i32, y: i32) -> i32 = x + y";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(defn add [(x i32) (y i32)] i32 (+ x y))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse annotating multiple bindings with no return type" {
    const allocator = std.testing.allocator;
    const source = "add(x: i32, y: i32) = x + y";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(defn add [(x i32) (y i32)] (+ x y))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiple parameters annotating only return type" {
    const allocator = std.testing.allocator;
    const source = "add(x, y) -> i32 = x + y";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(defn add [x y] i32 (+ x y))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multiple parameters annotating one parameter and return type" {
    const allocator = std.testing.allocator;
    const source = "add(x: i32, y) -> i32 = x + y";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected = "(defn add [(x i32) y] i32 (+ x y))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "parse multi line function" {
    const allocator = std.testing.allocator;
    const source =
        \\sum_squares(x: i32, y: i32) -> i32 =
        \\    x_squared = x ^ 2
        \\    y_squared = y ^ 2
        \\    x_squared + y_squared
    ;
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    const actual = try atom.parser.toString(allocator, intern, ast);
    defer allocator.free(actual);
    const expected =
        \\(defn sum_squares [(x i32) (y i32)] i32
        \\  (block
        \\    (def x_squared (^ x 2))
        \\    (def y_squared (^ y 2))
        \\    (+ x_squared y_squared)))
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "type infer discover return type" {
    const allocator = std.testing.allocator;
    const source = "id(x: i32) = x";
    var intern = atom.interner.Intern.init(allocator);
    defer intern.deinit();
    const builtins = try atom.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try atom.parser.parse(allocator, tokens);
    defer ast.deinit();
    var typed_ast = try atom.type_infer.TypedAst.init(allocator, ast);
    defer typed_ast.deinit();
    var constraints = atom.type_infer.Constraints.init(allocator);
    defer constraints.deinit();
    const id = try atom.interner.store(&intern, "id");
    var types = atom.type_infer.Types.init(allocator);
    defer types.deinit();
    try atom.type_infer.constrain(allocator, &constraints, &typed_ast, &types, builtins, id);
    try atom.type_infer.constrain(allocator, &constraints, &typed_ast, &types, builtins, id);
    // const substitution = try atom.type_infer.solve(constraints);
    // try atom.type_infer.apply(substitution, typed_ast);
    // const actual = try atom.type_infer.toString(allocator, typed_ast);
    // defer allocator.free(actual);
    // const expected = "id(x: i32) -> i32 = x";
    // try std.testing.expectEqualStrings(expected, actual);
}
