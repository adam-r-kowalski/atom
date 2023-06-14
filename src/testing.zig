const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("interner.zig");
const Intern = interner.Intern;
const Builtins = @import("builtins.zig").Builtins;
const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");
const type_checker = @import("type_checker.zig");
const code_generator = @import("code_generator.zig");
const error_reporter = @import("error_reporter.zig");

pub fn tokenize(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    var reconstructed = List(u8).init(arena.allocator());
    try tokenizer.source.tokens(tokens, reconstructed.writer());
    std.mem.replaceScalar(u8, reconstructed.items, '\t', ' ');
    var replaced_source = try arena.allocator().dupe(u8, source);
    std.mem.replaceScalar(u8, replaced_source, '\t', ' ');
    try std.testing.expectEqualStrings(replaced_source, reconstructed.items);
    var result = List(u8).init(allocator);
    try tokenizer.pretty_print.tokens(tokens, result.writer());
    return try result.toOwnedSlice();
}

pub fn parse(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const module = try parser.parse(arena.allocator(), tokens);
    var result = List(u8).init(allocator);
    try parser.pretty_print.module(module, result.writer());
    return try result.toOwnedSlice();
}

pub fn typeInfer(allocator: Allocator, source: []const u8, name: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var compile_errors = error_reporter.types.Errors.init(arena.allocator(), source);
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const untyped_ast = try parser.parse(arena.allocator(), tokens);
    var constraints = type_checker.types.Constraints{
        .equal = List(type_checker.types.EqualConstraint).init(arena.allocator()),
        .next_type_var = .{ .value = 0 },
    };
    var ast = try type_checker.infer.module(arena.allocator(), &constraints, builtins, untyped_ast);
    try type_checker.infer.topLevel(&ast, try intern.store(name), &compile_errors);
    const substitution = try type_checker.solve_constraints.constraints(arena.allocator(), constraints, &compile_errors);
    type_checker.apply_substitution.module(substitution, &ast);
    var result = List(u8).init(allocator);
    try type_checker.pretty_print.module(ast, result.writer());
    return try result.toOwnedSlice();
}

pub fn codegen(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var compile_errors = error_reporter.types.Errors.init(arena.allocator(), source);
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const untyped_ast = try parser.parse(arena.allocator(), tokens);
    var constraints = type_checker.types.Constraints{
        .equal = List(type_checker.types.EqualConstraint).init(arena.allocator()),
        .next_type_var = .{ .value = 0 },
    };
    var ast = try type_checker.infer.module(arena.allocator(), &constraints, builtins, untyped_ast);
    const export_count = ast.foreign_exports.len;
    const start = try intern.store("start");
    if (export_count == 0) ast.foreign_exports = &.{start};
    for (ast.foreign_exports) |foreign_export| try type_checker.infer.topLevel(&ast, foreign_export, &compile_errors);
    const substitution = try type_checker.solve_constraints.constraints(arena.allocator(), constraints, &compile_errors);
    type_checker.apply_substitution.module(substitution, &ast);
    var ir = try code_generator.lower.module(arena.allocator(), builtins, ast, &intern);
    if (export_count == 0) {
        const alias = try intern.store("_start");
        ir.foreign_exports = &.{.{ .name = start, .alias = alias }};
    }
    var result = List(u8).init(allocator);
    try code_generator.pretty_print.module(ir, result.writer());
    return try result.toOwnedSlice();
}

fn endToEnd(allocator: Allocator, intern: *Intern, errors: *error_reporter.types.Errors, source: []const u8) ![]const u8 {
    const builtins = try Builtins.init(intern);
    const tokens = try tokenizer.tokenize(allocator, intern, builtins, source);
    const untyped_ast = try parser.parse(allocator, tokens);
    var constraints = type_checker.types.Constraints{
        .equal = List(type_checker.types.EqualConstraint).init(allocator),
        .next_type_var = .{ .value = 0 },
    };
    var ast = try type_checker.infer.module(allocator, &constraints, builtins, untyped_ast);
    const export_count = ast.foreign_exports.len;
    const start = try intern.store("start");
    if (export_count == 0) ast.foreign_exports = &.{start};
    for (ast.foreign_exports) |foreign_export| try type_checker.infer.topLevel(&ast, foreign_export, errors);
    const substitution = try type_checker.solve_constraints.constraints(allocator, constraints, errors);
    type_checker.apply_substitution.module(substitution, &ast);
    var ir = try code_generator.lower.module(allocator, builtins, ast, intern);
    if (export_count == 0) {
        const alias = try intern.store("_start");
        ir.foreign_exports = &.{.{ .name = start, .alias = alias }};
    }
    return try std.fmt.allocPrint(allocator, "{}", .{ir});
}

pub fn compileErrors(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var compile_errors = error_reporter.types.Errors.init(arena.allocator(), source);
    _ = endToEnd(arena.allocator(), &intern, &compile_errors, source) catch |e| {
        std.debug.assert(e == error.CompileError);
        var result = List(u8).init(allocator);
        try compile_errors.pretty_print(result.writer());
        return try result.toOwnedSlice();
    };
    std.debug.panic("\nexpected compile error", .{});
}
