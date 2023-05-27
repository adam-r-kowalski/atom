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
    const reconstructed = try tokenizer.toSource(arena.allocator(), intern, tokens);
    var replaced_source = try arena.allocator().dupe(u8, source);
    var replaced_reconstructed = try arena.allocator().dupe(u8, reconstructed);
    std.mem.replaceScalar(u8, replaced_source, '\t', ' ');
    std.mem.replaceScalar(u8, replaced_reconstructed, '\t', ' ');
    try std.testing.expectEqualStrings(replaced_source, replaced_reconstructed);
    return try tokenizer.toString(allocator, intern, tokens);
}

pub fn parse(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const module = try parser.parse(arena.allocator(), tokens);
    return try parser.toString(allocator, intern, module);
}

pub fn typeInfer(allocator: Allocator, source: []const u8, name: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const untyped_module = try parser.parse(arena.allocator(), tokens);
    var module = try type_checker.infer.module(arena.allocator(), builtins, untyped_module);
    const interned = try intern.store(name);
    var constraints = type_checker.types.Constraints{
        .equal = List(type_checker.types.Equal).init(arena.allocator()),
    };
    var next_type_var: type_checker.types.TypeVar = 0;
    try type_checker.infer.infer(arena.allocator(), &constraints, &module, builtins, &next_type_var, interned);
    const substitution = try type_checker.solve(arena.allocator(), constraints);
    const typed_module = try type_checker.apply(arena.allocator(), substitution, module);
    var list = List(u8).init(allocator);
    const writer = list.writer();
    try type_checker.toString(writer, intern, typed_module);
    return list.toOwnedSlice();
}

pub fn typeInferVerbose(allocator: Allocator, source: []const u8, name: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const untyped_module = try parser.parse(arena.allocator(), tokens);
    const interned = try intern.store(name);
    var next_type_var: type_checker.types.TypeVar = 0;
    var module = try type_checker.infer.module(arena.allocator(), builtins, untyped_module, &next_type_var);
    var constraints = type_checker.types.Constraints{
        .equal = List(type_checker.types.Equal).init(arena.allocator()),
    };
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
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const untyped_module = try parser.parse(arena.allocator(), tokens);
    var module = try type_checker.infer.module(arena.allocator(), builtins, untyped_module);
    var constraints = type_checker.types.Constraints{
        .equal = List(type_checker.types.Equal).init(arena.allocator()),
    };
    const start = try intern.store("start");
    var next_type_var: type_checker.types.TypeVar = 0;
    try type_checker.infer.infer(arena.allocator(), &constraints, &module, builtins, &next_type_var, start);
    const substitution = try type_checker.solve(arena.allocator(), constraints);
    const typed_module = try type_checker.apply(arena.allocator(), substitution, module);
    var ir = try lower.buildIr(arena.allocator(), builtins, typed_module);
    const alias = try intern.store("_start");
    const exports = try arena.allocator().alloc(lower.types.Export, ir.exports.len + 1);
    std.mem.copy(lower.types.Export, exports, ir.exports);
    exports[ir.exports.len] = lower.types.Export{ .name = start, .alias = alias };
    ir.exports = exports;
    return try wat(allocator, intern, ir);
}
