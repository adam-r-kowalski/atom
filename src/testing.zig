const std = @import("std");
const Allocator = std.mem.Allocator;

const interner = @import("interner.zig");
const Intern = interner.Intern;
const Builtins = @import("builtins.zig").Builtins;
const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");
const type_infer = @import("type_infer.zig");

pub fn tokenize(allocator: Allocator, source: []const u8) ![]const u8 {
    var intern = Intern.init(allocator);
    defer intern.deinit();
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const reconstructed = try tokenizer.toSource(allocator, intern, tokens);
    defer allocator.free(reconstructed);
    try std.testing.expectEqualStrings(source, reconstructed);
    return try tokenizer.toString(allocator, intern, tokens);
}

pub fn parse(allocator: Allocator, source: []const u8) ![]const u8 {
    var intern = Intern.init(allocator);
    defer intern.deinit();
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try parser.parse(allocator, tokens);
    defer ast.deinit();
    return try parser.toString(allocator, intern, ast);
}

pub fn typeInfer(allocator: Allocator, source: []const u8, name: []const u8) ![]const u8 {
    var intern = Intern.init(allocator);
    defer intern.deinit();
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(allocator, &intern, builtins, source);
    defer tokens.deinit();
    const ast = try parser.parse(allocator, tokens);
    defer ast.deinit();
    var typed_ast = try type_infer.TypedAst.init(allocator, ast);
    defer typed_ast.deinit();
    var constraints = type_infer.Constraints.init(allocator);
    defer constraints.deinit();
    const id = try interner.store(&intern, name);
    var types = type_infer.Types.init(allocator);
    defer types.deinit();
    try type_infer.constrain(allocator, &constraints, &typed_ast, &types, builtins, id);
    var substitution = try type_infer.solve(allocator, types, constraints);
    defer substitution.deinit();
    type_infer.apply(substitution, &types);
    return try type_infer.toString(allocator, intern, typed_ast, types);
}
