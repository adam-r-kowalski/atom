const std = @import("std");
const fusion = @import("fusion");

test "parse call" {
    const allocator = std.testing.allocator;
    const source =
        \\f x y z
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
        \\(f x y z)
    ;
    try std.testing.expectEqualStrings(expected, actual);
}

test "binary operators don't get included in call" {
    const allocator = std.testing.allocator;
    for ([_][]const u8{ "+", "-", "*", "^", ">", "<" }) |op| {
        const source = try std.fmt.allocPrint(allocator, "f x {s} f y", .{op});
        defer allocator.free(source);
        var intern = fusion.Intern.init(allocator);
        defer intern.deinit();
        const builtins = try fusion.tokenizer.Builtins.init(&intern);
        const tokens = try fusion.tokenizer.tokenize(allocator, &intern, builtins, source);
        defer tokens.deinit();
        const ast = try fusion.parser.parse(allocator, tokens);
        defer ast.deinit();
        const actual = try fusion.parser.toString(allocator, intern, ast);
        defer allocator.free(actual);
        const expected = try std.fmt.allocPrint(allocator, "({s} (f x) (f y))", .{op});
        defer allocator.free(expected);
        try std.testing.expectEqualStrings(expected, actual);
    }
}
