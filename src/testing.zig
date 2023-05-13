const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("interner.zig");
const Intern = interner.Intern;
const Builtins = @import("builtins.zig").Builtins;
const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");
const type_checker = @import("type_checker.zig");

pub fn tokenize(allocator: Allocator, source: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var intern = Intern.init(arena.allocator());
    const builtins = try Builtins.init(&intern);
    const tokens = try tokenizer.tokenize(arena.allocator(), &intern, builtins, source);
    const reconstructed = try tokenizer.toSource(arena.allocator(), intern, tokens);
    try std.testing.expectEqualStrings(source, reconstructed);
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
    var module = try type_checker.infer.module(arena.allocator(), untyped_module);
    const interned = try interner.store(&intern, name);
    var next_type_var: type_checker.types.TypeVar = 0;
    try type_checker.infer.infer(allocator, &module, builtins, &next_type_var, interned);
    return try type_checker.toString(allocator, intern, module);
}
