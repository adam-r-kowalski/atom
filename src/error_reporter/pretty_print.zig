const std = @import("std");
const List = std.ArrayList;
const Writer = List(u8).Writer;

const type_checker = @import("../type_checker.zig");
const types = @import("types.zig");
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

pub fn undefinedVariable(u: types.UndefinedVariable, lines: [][]const u8, writer: Writer) !void {
    try writer.print(
        \\--- UNDEFINED VARIABLE ---------------------------------------------------
        \\
        \\Cannot find variable `{}`.
        \\
        \\
    , .{u.symbol});
    try source(lines, u.span, writer);
    try writer.writeAll(
        \\
        \\
        \\Maybe you want one of the following?
        \\
    );
    for (u.in_scope) |symbol| try writer.print("\n    {}", .{symbol});
    try writer.writeAll(
        \\
        \\
    );
}

fn typeMismatch(t: types.TypeMismatch, lines: [][]const u8, writer: Writer) !void {
    try writer.writeAll(
        \\--- TYPE MISMATCH ---------------------------------------------------
        \\
        \\Here the inferred type is 
    );
    try type_checker.pretty_print.monotype(t.left.type, writer);
    try writer.writeAll("\n\n");
    if (t.left.span) |span| try source(lines, span, writer);
    try writer.writeAll(
        \\
        \\
        \\Here the inferred type is 
    );
    try type_checker.pretty_print.monotype(t.right.type, writer);
    try writer.writeAll("\n\n");
    if (t.right.span) |span| try source(lines, span, writer);
    try writer.writeAll(
        \\
        \\
        \\Expected these two types to be the same.
        \\
        \\
    );
}

pub fn errors(es: types.Errors, writer: Writer) !void {
    var lines = List([]const u8).init(es.allocator);
    var iterator = std.mem.split(u8, es.source, "\n");
    while (iterator.next()) |line| try lines.append(line);
    for (es.undefined_variables.items) |u| try undefinedVariable(u, lines.items, writer);
    for (es.type_mismatches.items) |u| try typeMismatch(u, lines.items, writer);
}
