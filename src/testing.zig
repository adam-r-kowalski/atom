const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("interner.zig");
const Intern = interner.Intern;
const Builtins = @import("builtins.zig").Builtins;
const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");
const type_checker = @import("type_checker.zig");
const lower = @import("lower.zig");
const Constraints = @import("constraints.zig").Constraints;
const TypeVar = @import("substitution.zig").TypeVar;
const Module = @import("typed_ast.zig").Module;
const CompileErrors = @import("compile_errors.zig").CompileErrors;

pub fn tokenize(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var compile_errors = CompileErrors.init(arena.allocator(), source);
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, &compile_errors, builtins, source);
    const reconstructed = try tokens.toSource(arena.allocator());
    var replaced_source = try arena.allocator().dupe(u8, source);
    var replaced_reconstructed = try arena.allocator().dupe(u8, reconstructed);
    std.mem.replaceScalar(u8, replaced_source, '\t', ' ');
    std.mem.replaceScalar(u8, replaced_reconstructed, '\t', ' ');
    try std.testing.expectEqualStrings(replaced_source, replaced_reconstructed);
    return try std.fmt.allocPrint(allocator, "{}", .{tokens});
}

pub fn parse(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var compile_errors = CompileErrors.init(arena.allocator(), source);
    const builtins = try Builtins.init(&intern);
    var tokens = try tokenizer.tokenize(arena.allocator(), &intern, &compile_errors, builtins, source);
    const ast = try parser.parse(arena.allocator(), &tokens);
    return try std.fmt.allocPrint(allocator, "{indent 3}", .{ast});
}

pub fn typeInfer(allocator: Allocator, source: []const u8, name: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var compile_errors = CompileErrors.init(arena.allocator(), source);
    const builtins = try Builtins.init(&intern);
    var tokens = try tokenizer.tokenize(arena.allocator(), &intern, &compile_errors, builtins, source);
    const untyped_ast = try parser.parse(arena.allocator(), &tokens);
    var constraints = Constraints.init(arena.allocator());
    var ast = try Module.init(arena.allocator(), &constraints, builtins, untyped_ast);
    try type_checker.infer(&ast, try intern.store(name));
    const substitution = try constraints.solve(arena.allocator());
    ast.apply(substitution);
    return try std.fmt.allocPrint(allocator, "{}", .{ast});
}

pub fn typeInferVerbose(allocator: Allocator, source: []const u8, name: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var compile_errors = CompileErrors.init(arena.allocator(), source);
    const builtins = try Builtins.init(&intern);
    var tokens = try tokenizer.tokenize(arena.allocator(), &intern, &compile_errors, builtins, source);
    const untyped_ast = try parser.parse(arena.allocator(), &tokens);
    var constraints = Constraints.init(arena.allocator());
    var ast = try Module.init(arena.allocator(), &constraints, builtins, untyped_ast);
    try type_checker.infer(&ast, try intern.store(name));
    const substitution = try constraints.solve(arena.allocator());
    ast.apply(substitution);
    return try std.fmt.allocPrint(allocator,
        \\{}
        \\{}
        \\{}
    , .{ ast, constraints, substitution, intern });
}

pub fn codegen(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var compile_errors = CompileErrors.init(arena.allocator(), source);
    const builtins = try Builtins.init(&intern);
    var tokens = try tokenizer.tokenize(arena.allocator(), &intern, &compile_errors, builtins, source);
    const untyped_ast = try parser.parse(arena.allocator(), &tokens);
    var constraints = Constraints.init(arena.allocator());
    var ast = try Module.init(arena.allocator(), &constraints, builtins, untyped_ast);
    const start = try intern.store("start");
    try type_checker.infer(&ast, start);
    const substitution = try constraints.solve(arena.allocator());
    ast.apply(substitution);
    var ir = try lower.buildIr(arena.allocator(), builtins, ast);
    const alias = try intern.store("_start");
    ir.exports = &.{.{ .name = start, .alias = alias }};
    return try std.fmt.allocPrint(allocator, "{}", .{ir});
}

fn endToEnd(allocator: Allocator, intern: *Intern, compile_errors: *CompileErrors, source: []const u8) ![]const u8 {
    const builtins = try Builtins.init(intern);
    var tokens = try tokenizer.tokenize(allocator, intern, compile_errors, builtins, source);
    const untyped_ast = try parser.parse(allocator, &tokens);
    var constraints = Constraints.init(allocator);
    var ast = try Module.init(allocator, &constraints, builtins, untyped_ast);
    const start = try intern.store("start");
    try type_checker.infer(&ast, start);
    const substitution = try constraints.solve(allocator);
    ast.apply(substitution);
    var ir = try lower.buildIr(allocator, builtins, ast);
    const alias = try intern.store("_start");
    ir.exports = &.{.{ .name = start, .alias = alias }};
    return try std.fmt.allocPrint(allocator, "{}", .{ir});
}

pub fn compileErrors(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    var compile_errors = CompileErrors.init(arena.allocator(), source);
    _ = endToEnd(arena.allocator(), &intern, &compile_errors, source) catch |e| {
        std.debug.assert(e == error.CompileError);
        return try std.fmt.allocPrint(allocator, "{}", .{compile_errors});
    };
    std.debug.panic("\nexpected compile error", .{});
}
