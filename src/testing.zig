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
    const module = try parser.parse(arena.allocator(), builtins, tokens);
    var result = List(u8).init(allocator);
    try parser.pretty_print.module(module, result.writer());
    return try result.toOwnedSlice();
}

pub fn typeInfer(allocator: Allocator, source: []const u8, name: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var errors = error_reporter.types.Errors{
        .allocator = arena.allocator(),
        .undefined_variable = List(error_reporter.types.UndefinedVariable).init(arena.allocator()),
        .type_mismatch = List(error_reporter.types.TypeMismatch).init(arena.allocator()),
        .mutability_mismatch = List(error_reporter.types.MutabilityMismatch).init(arena.allocator()),
        .reassigning_immutable = List(error_reporter.types.ReassigningImmutable).init(arena.allocator()),
        .source = source,
    };
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const untyped_ast = try parser.parse(arena.allocator(), builtins, tokens);
    var constraints = type_checker.types.Constraints{
        .equal = List(type_checker.types.EqualConstraint).init(arena.allocator()),
        .next_type_var = 0,
    };
    var ast = try type_checker.infer.module(arena.allocator(), &constraints, builtins, &intern, untyped_ast);
    try type_checker.infer.topLevel(&ast, try intern.store(name), &errors);
    const substitution = try type_checker.solve_constraints.constraints(arena.allocator(), constraints, &errors);
    ast = try type_checker.apply_substitution.module(arena.allocator(), substitution, ast);
    var result = List(u8).init(allocator);
    try type_checker.pretty_print.module(ast, result.writer());
    return try result.toOwnedSlice();
}

pub fn typeInferVerbose(allocator: Allocator, source: []const u8, name: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var errors = error_reporter.types.Errors{
        .allocator = arena.allocator(),
        .undefined_variable = List(error_reporter.types.UndefinedVariable).init(arena.allocator()),
        .type_mismatch = List(error_reporter.types.TypeMismatch).init(arena.allocator()),
        .mutability_mismatch = List(error_reporter.types.MutabilityMismatch).init(arena.allocator()),
        .reassigning_immutable = List(error_reporter.types.ReassigningImmutable).init(arena.allocator()),
        .source = source,
    };
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const untyped_ast = try parser.parse(arena.allocator(), builtins, tokens);
    var constraints = type_checker.types.Constraints{
        .equal = List(type_checker.types.EqualConstraint).init(arena.allocator()),
        .next_type_var = 0,
    };
    var ast = try type_checker.infer.module(arena.allocator(), &constraints, builtins, untyped_ast);
    try type_checker.infer.topLevel(&ast, try intern.store(name), &errors);
    const substitution = try type_checker.solve_constraints.constraints(arena.allocator(), constraints, &errors);
    const typed_ast = try type_checker.apply_substitution.module(arena.allocator(), substitution, ast);
    var result = List(u8).init(allocator);
    const writer = result.writer();
    try writer.writeAll("\n\n======== Untyped ========\n\n");
    try type_checker.pretty_print.module(ast, writer);
    try writer.writeAll("\n\n======== Constraints ========\n\n");
    try type_checker.pretty_print.constraints(constraints, writer);
    try writer.writeAll("\n\n======== Substitution ========\n\n");
    try type_checker.pretty_print.substitution(substitution, writer);
    try writer.writeAll("\n\n======== Typed ========\n\n");
    try type_checker.pretty_print.module(typed_ast, writer);
    return try result.toOwnedSlice();
}

pub fn codegen(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var errors = error_reporter.types.Errors{
        .allocator = arena.allocator(),
        .undefined_variable = List(error_reporter.types.UndefinedVariable).init(arena.allocator()),
        .type_mismatch = List(error_reporter.types.TypeMismatch).init(arena.allocator()),
        .mutability_mismatch = List(error_reporter.types.MutabilityMismatch).init(arena.allocator()),
        .reassigning_immutable = List(error_reporter.types.ReassigningImmutable).init(arena.allocator()),
        .source = source,
    };
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const untyped_ast = try parser.parse(arena.allocator(), builtins, tokens);
    var constraints = type_checker.types.Constraints{
        .equal = List(type_checker.types.EqualConstraint).init(arena.allocator()),
        .next_type_var = 0,
    };
    var ast = try type_checker.infer.module(arena.allocator(), &constraints, builtins, &intern, untyped_ast);
    const export_count = ast.foreign_exports.len;
    const start = try intern.store("start");
    if (export_count == 0) ast.foreign_exports = &.{start};
    for (ast.foreign_exports) |foreign_export| try type_checker.infer.topLevel(&ast, foreign_export, &errors);
    const substitution = try type_checker.solve_constraints.constraints(arena.allocator(), constraints, &errors);
    ast = try type_checker.apply_substitution.module(arena.allocator(), substitution, ast);
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
    const untyped_ast = try parser.parse(allocator, builtins, tokens);
    var constraints = type_checker.types.Constraints{
        .equal = List(type_checker.types.EqualConstraint).init(allocator),
        .next_type_var = 0,
    };
    var ast = try type_checker.infer.module(allocator, &constraints, builtins, intern, untyped_ast);
    const export_count = ast.foreign_exports.len;
    const start = try intern.store("start");
    if (export_count == 0) ast.foreign_exports = &.{start};
    for (ast.foreign_exports) |foreign_export| try type_checker.infer.topLevel(&ast, foreign_export, errors);
    const substitution = try type_checker.solve_constraints.constraints(allocator, constraints, errors);
    ast = try type_checker.apply_substitution.module(allocator, substitution, ast);
    var ir = try code_generator.lower.module(allocator, builtins, ast, intern);
    if (export_count == 0) {
        const alias = try intern.store("_start");
        ir.foreign_exports = &.{.{ .name = start, .alias = alias }};
    }
    var result = List(u8).init(allocator);
    try code_generator.pretty_print.module(ir, result.writer());
    return try result.toOwnedSlice();
}

pub fn compileErrors(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var errors = error_reporter.types.Errors{
        .allocator = arena.allocator(),
        .undefined_variable = List(error_reporter.types.UndefinedVariable).init(arena.allocator()),
        .type_mismatch = List(error_reporter.types.TypeMismatch).init(arena.allocator()),
        .mutability_mismatch = List(error_reporter.types.MutabilityMismatch).init(arena.allocator()),
        .reassigning_immutable = List(error_reporter.types.ReassigningImmutable).init(arena.allocator()),
        .source = source,
    };
    _ = endToEnd(arena.allocator(), &intern, &errors, source) catch |e| {
        std.debug.assert(e == error.CompileError);
        var result = List(u8).init(allocator);
        try error_reporter.pretty_print.errors(errors, result.writer());
        return try result.toOwnedSlice();
    };
    std.debug.panic("\nexpected compile error", .{});
}
