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
const wat = @import("codegen.zig").wat;

pub fn tokenize(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
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
    const builtins = try Builtins.init(&intern);
    var tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const ast = try parser.parse(arena.allocator(), &tokens);
    return try std.fmt.allocPrint(allocator, "{indent 3}", .{ast});
}

pub fn typeInfer(allocator: Allocator, source: []const u8, name: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    const builtins = try Builtins.init(&intern);
    var tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const untyped_ast = try parser.parse(arena.allocator(), &tokens);
    var constraints = type_checker.Constraints.init(arena.allocator());
    var next_type_var: type_checker.TypeVar = 0;
    var ast = try type_checker.Ast.init(arena.allocator(), &constraints, &next_type_var, builtins, untyped_ast);
    try ast.infer(name);
    const substitution = try constraints.solve(arena.allocator());
    ast.apply(substitution);
    return try std.fmt.allocPrint(allocator, "{}", .{ast});
}

pub fn typeInferVerbose(allocator: Allocator, source: []const u8, name: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    const builtins = try Builtins.init(&intern);
    var tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const untyped_module = try parser.parse(arena.allocator(), &tokens);
    const interned = try intern.store(name);
    var next_type_var: type_checker.TypeVar = 0;
    var module = try type_checker.infer.module(arena.allocator(), builtins, untyped_module, &next_type_var);
    var constraints = type_checker.Constraints.init(arena.allocator());
    try type_checker.infer.infer(arena.allocator(), &constraints, &module, builtins, &next_type_var, interned);
    const substitution = try type_checker.solve(arena.allocator(), constraints);
    const typed_module = try type_checker.apply(arena.allocator(), substitution, module);
    var list = List(u8).init(allocator);
    const writer = list.writer();
    try type_checker.to_verbose_string.module(writer, intern, module);
    try type_checker.to_verbose_string.constraints(writer, constraints);
    try type_checker.to_verbose_string.substitution(writer, substitution);
    try type_checker.to_verbose_string.module(writer, intern, typed_module);
    return list.toOwnedSlice();
}

pub fn codegen(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    const builtins = try Builtins.init(&intern);
    var tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const untyped_ast = try parser.parse(arena.allocator(), &tokens);
    var constraints = type_checker.Constraints.init(arena.allocator());
    var next_type_var: type_checker.TypeVar = 0;
    var ast = try type_checker.Ast.init(arena.allocator(), &constraints, &next_type_var, builtins, untyped_ast);
    try ast.infer("start");
    const substitution = try constraints.solve(arena.allocator());
    ast.apply(substitution);
    var ir = try lower.buildIr(arena.allocator(), builtins, ast);
    const start = try intern.store("start");
    const alias = try intern.store("_start");
    const exports = try arena.allocator().alloc(lower.Export, ir.exports.len + 1);
    std.mem.copy(lower.Export, exports, ir.exports);
    exports[ir.exports.len] = lower.Export{ .name = start, .alias = alias };
    ir.exports = exports;
    return try wat(allocator, ir);
}
