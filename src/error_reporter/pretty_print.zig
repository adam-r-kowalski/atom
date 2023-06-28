const std = @import("std");
const List = std.ArrayList;
const Writer = List(u8).Writer;

const type_checker = @import("../type_checker.zig");
const types = @import("types.zig");
const edit_distance = @import("../edit_distance.zig");
pub const RED = "\x1b[31m";
pub const CLEAR = "\x1b[0m";

fn source(lines: [][]const u8, span: type_checker.types.Span, writer: Writer) !void {
    var index = span.begin.line - 1;
    if (index > 0) try writer.print("{} | {s}\n", .{ span.begin.line - 1, lines[index - 1] });
    const line = lines[index];
    try writer.print("{} | {s}", .{ span.begin.line, line[0 .. span.begin.column - 1] });
    try writer.writeAll(RED);
    if (span.begin.line == span.end.line) {
        try writer.writeAll(line[span.begin.column - 1 .. span.end.column - 1]);
        try writer.writeAll(CLEAR);
        try writer.writeAll(line[span.end.column - 1 ..]);
    } else {
        try writer.writeAll(line[span.begin.column - 1 ..]);
        for (span.begin.line..span.end.line) |i| {
            try writer.print("\n{s}{} |{s} {s}", .{ CLEAR, i + 1, RED, lines[i] });
        }
        try writer.writeAll(CLEAR);
    }
    index = span.end.line - 1;
    if (index < lines.len - 1) try writer.print("\n{} | {s}", .{ span.end.line + 1, lines[index + 1] });
}

pub fn undefinedVariable(allocator: std.mem.Allocator, u: types.UndefinedVariable, lines: [][]const u8, writer: Writer) !void {
    try writer.print(
        \\---- {s}UNDEFINED VARIABLE{s} ---------------------------------------------------
        \\
        \\Cannot find variable `{s}`.
        \\
        \\
    , .{ RED, CLEAR, u.symbol.string() });
    try source(lines, u.span, writer);
    try writer.writeAll(
        \\
        \\
        \\Maybe you want one of the following?
        \\
    );
    const strings = try allocator.alloc([]const u8, u.in_scope.len);
    for (u.in_scope, strings) |symbol, *string| string.* = symbol.string();
    const suggestions = try edit_distance.sort(allocator, u.symbol.string(), strings);
    for (suggestions) |suggestion| try writer.print("\n    {s}", .{suggestion.text});
    try writer.writeAll(
        \\
        \\
    );
}

fn typeMismatch(t: types.TypeMismatch, lines: [][]const u8, writer: Writer) !void {
    try writer.print(
        \\---- {s}TYPE MISMATCH{s} ---------------------------------------------------
        \\
        \\Here the inferred type is 
    , .{ RED, CLEAR });
    try type_checker.pretty_print.monotype(t.left, writer);
    try writer.writeAll("\n\n");
    if (type_checker.monotype.span(t.left)) |span| try source(lines, span, writer);
    try writer.writeAll(
        \\
        \\
        \\
        \\Here the inferred type is 
    );
    try type_checker.pretty_print.monotype(t.right, writer);
    try writer.writeAll("\n\n");
    if (type_checker.monotype.span(t.right)) |span| try source(lines, span, writer);
    try writer.writeAll(
        \\
        \\
        \\
        \\Expected these two types to be the same.
        \\
        \\
    );
}

fn mutabilityMismatch(m: types.MutabilityMismatch, lines: [][]const u8, writer: Writer) !void {
    try writer.print(
        \\---- {s}MUTABILITY MISMATCH{s} ---------------------------------------------------
        \\
        \\Here we have a {s} value
    , .{ RED, CLEAR, if (m.left.mutable) "mutable" else "immutable" });
    try writer.writeAll("\n\n");
    if (m.left.span) |span| try source(lines, span, writer);
    try writer.print(
        \\
        \\
        \\
        \\Here we have a {s} value
    , .{if (m.right.mutable) "mutable" else "immutable"});
    try writer.writeAll("\n\n");
    if (m.right.span) |span| try source(lines, span, writer);
    try writer.writeAll(
        \\
        \\
        \\
        \\Expected both of these values to be mutable.
        \\
        \\
    );
}

fn reassigningImmutable(m: types.ReassigningImmutable, lines: [][]const u8, writer: Writer) !void {
    try writer.print(
        \\---- {s}REASSIGNING IMMUTABLE VALUE{s} ---------------------------------------------------
        \\
        \\Cannot reassign immutable value `{s}{s}{s}`.
        \\
        \\
    , .{ RED, CLEAR, RED, m.name.string(), CLEAR });
    try source(lines, m.span, writer);
    try writer.writeAll(
        \\
        \\
        \\Perhaps you meant to make this value mutable?
        \\
        \\
    );
}

pub fn errors(es: types.Errors, writer: Writer) !void {
    var lines = List([]const u8).init(es.allocator);
    var iterator = std.mem.split(u8, es.source, "\n");
    while (iterator.next()) |line| try lines.append(line);
    for (es.undefined_variable.items) |u| try undefinedVariable(es.allocator, u, lines.items, writer);
    for (es.type_mismatch.items) |u| try typeMismatch(u, lines.items, writer);
    for (es.mutability_mismatch.items) |u| try mutabilityMismatch(u, lines.items, writer);
    for (es.reassigning_immutable.items) |u| try reassigningImmutable(u, lines.items, writer);
}
