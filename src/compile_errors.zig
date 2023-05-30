const std = @import("std");
const List = std.ArrayList;
const Allocator = std.mem.Allocator;

const Interned = @import("interner.zig").Interned;
const Span = @import("span.zig").Span;

fn writeSource(lines: [][]const u8, span: Span, writer: anytype) !void {
    const index = span.begin.line - 1;
    if (index > 0) try writer.print("{} | {s}\n", .{ span.begin.line - 1, lines[index - 1] });
    const line = lines[index];
    try writer.print("{} | {s}", .{ span.begin.line, line[0 .. span.begin.column - 1] });
    try writer.writeAll("\x1b[31m");
    try writer.writeAll(line[span.begin.column - 1 .. span.end.column - 1]);
    try writer.writeAll("\x1b[0m");
    try writer.writeAll(line[span.end.column - 1 ..]);
    if (index < lines.len - 1) try writer.print("\n{} | {s}", .{ span.begin.line + 1, lines[index + 1] });
}

pub const UndefinedVariable = struct {
    symbol: Interned,
    span: Span,
    in_scope: []const Interned,

    fn toString(self: UndefinedVariable, lines: [][]const u8, writer: anytype) !void {
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
            \\
        );
        for (self.in_scope) |symbol| {
            try writer.print("    {}", .{symbol});
        }
        try writer.writeAll(
            \\
            \\
        );
    }
};

pub const CompileError = union(enum) {
    undefined_variable: UndefinedVariable,

    fn toString(self: CompileError, lines: [][]const u8, writer: anytype) !void {
        switch (self) {
            .undefined_variable => try self.undefined_variable.toString(lines, writer),
        }
    }
};

pub const CompileErrors = struct {
    allocator: Allocator,
    errors: List(CompileError),
    source: []const u8,

    pub fn init(allocator: Allocator, source: []const u8) CompileErrors {
        return CompileErrors{
            .allocator = allocator,
            .errors = List(CompileError).init(allocator),
            .source = source,
        };
    }

    pub fn format(self: CompileErrors, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        var lines = List([]const u8).init(self.allocator);
        var iterator = std.mem.split(u8, self.source, "\n");
        while (iterator.next()) |line| lines.append(line) catch unreachable;
        for (self.errors.items) |e| e.toString(lines.items, writer) catch unreachable;
    }
};
