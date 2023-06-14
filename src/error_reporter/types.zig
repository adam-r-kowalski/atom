const std = @import("std");
const List = std.ArrayList;
const Allocator = std.mem.Allocator;

const Interned = @import("../interner.zig").Interned;
const Span = @import("../tokenizer.zig").types.Span;
const type_checker = @import("../type_checker.zig");
pub const RED = "\x1b[31m";
pub const CLEAR = "\x1b[0m";
const Writer = List(u8).Writer;

fn writeSource(lines: [][]const u8, span: Span, writer: Writer) !void {
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

pub const UndefinedVariable = struct {
    symbol: Interned,
    span: Span,
    in_scope: []const Interned,

    fn toString(self: UndefinedVariable, lines: [][]const u8, writer: Writer) !void {
        try writer.print(
            \\--- UNDEFINED VARIABLE ---------------------------------------------------
            \\
            \\Cannot find variable `{}`.
            \\
            \\
        , .{self.symbol});
        try writeSource(lines, self.span, writer);
        try writer.writeAll(
            \\
            \\
            \\Maybe you want one of the following?
            \\
        );
        for (self.in_scope) |symbol| try writer.print("\n    {}", .{symbol});
        try writer.writeAll(
            \\
            \\
        );
    }
};

pub const TypeError = struct {
    left: type_checker.types.TypedSpan,
    right: type_checker.types.TypedSpan,

    fn toString(self: TypeError, lines: [][]const u8, writer: Writer) !void {
        try writer.writeAll(
            \\--- TYPE ERROR ---------------------------------------------------
            \\
            \\Here the inferred type is 
        );
        try type_checker.pretty_print.monotype(self.left.type, writer);
        try writer.writeAll("\n\n");
        if (self.left.span) |span| try writeSource(lines, span, writer);
        try writer.writeAll(
            \\
            \\
            \\Here the inferred type is 
        );
        try type_checker.pretty_print.monotype(self.right.type, writer);
        try writer.writeAll("\n\n");
        if (self.right.span) |span| try writeSource(lines, span, writer);
        try writer.writeAll(
            \\
            \\
            \\Expected these two types to be the same.
            \\
            \\
        );
    }
};

pub const Error = union(enum) {
    undefined_variable: UndefinedVariable,
    type_error: TypeError,

    fn toString(self: Error, lines: [][]const u8, writer: Writer) !void {
        switch (self) {
            .undefined_variable => try self.undefined_variable.toString(lines, writer),
            .type_error => try self.type_error.toString(lines, writer),
        }
    }
};

pub const Errors = struct {
    allocator: Allocator,
    errors: List(Error),
    source: []const u8,

    pub fn init(allocator: Allocator, source: []const u8) Errors {
        return Errors{
            .allocator = allocator,
            .errors = List(Error).init(allocator),
            .source = source,
        };
    }

    pub fn pretty_print(self: Errors, writer: Writer) !void {
        var lines = List([]const u8).init(self.allocator);
        var iterator = std.mem.split(u8, self.source, "\n");
        while (iterator.next()) |line| lines.append(line) catch unreachable;
        for (self.errors.items) |e| e.toString(lines.items, writer) catch unreachable;
    }
};
