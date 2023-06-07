const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;

const interner = @import("interner.zig");
const Interned = interner.Interned;
const Intern = interner.Intern;
const Builtins = @import("builtins.zig").Builtins;
const token = @import("token.zig");
const Span = @import("span.zig").Span;
const Pos = @import("span.zig").Pos;
const Token = token.Token;
const Tokens = token.Tokens;
const CompileErrors = @import("compile_errors.zig").CompileErrors;

const Cursor = struct {
    source: []const u8,
    pos: Pos,
};

fn trim(cursor: *Cursor) void {
    var i: u64 = 0;
    while (i < cursor.source.len) {
        switch (cursor.source[i]) {
            ' ', '\t' => i += 1,
            else => break,
        }
    }
    cursor.pos.column += i;
    cursor.source = cursor.source[i..];
}

fn reserved(c: u8) bool {
    return switch (c) {
        ' ', '\n', '(', ')', '.', ':', ',' => true,
        else => false,
    };
}

fn advance(cursor: *Cursor, n: u64) []const u8 {
    const value = cursor.source[0..n];
    cursor.source = cursor.source[n..];
    cursor.pos.column += n;
    return value;
}

fn number(intern: *Intern, cursor: *Cursor) !Token {
    const begin = cursor.pos;
    var i: u64 = 1;
    var decimals: u64 = if (cursor.source[0] == '.') 1 else 0;
    while (i < cursor.source.len) : (i += 1) {
        switch (cursor.source[i]) {
            '0'...'9' => {},
            '.' => decimals += 1,
            else => break,
        }
    }
    if (i >= 2 and cursor.source[i - 1] == '.') {
        i -= 1;
        decimals -= 1;
    }
    const contents = cursor.source[0..i];
    if (contents.len == 1) {
        switch (contents[0]) {
            '-' => return exact(cursor, .minus),
            '.' => {
                _ = advance(cursor, i);
                return Token{ .dot = .{ .span = .{ .begin = begin, .end = cursor.pos } } };
            },
            else => {},
        }
    }
    _ = advance(cursor, i);
    const span = Span{ .begin = begin, .end = cursor.pos };
    const interned = try intern.store(contents);
    if (decimals == 0) return Token{ .int = .{ .value = interned, .span = span } };
    return Token{ .float = .{ .value = interned, .span = span } };
}

fn string(intern: *Intern, cursor: *Cursor) !Token {
    const begin = cursor.pos;
    var i: u64 = 1;
    while (i < cursor.source.len) : (i += 1) {
        if (cursor.source[i] == '"') {
            i += 1;
            break;
        }
    }
    const contents = cursor.source[0..i];
    _ = advance(cursor, i);
    const span = Span{ .begin = begin, .end = cursor.pos };
    const interned = try intern.store(contents);
    return Token{ .string = .{ .value = interned, .span = span } };
}

const Tag = std.meta.Tag(Token);

fn exact(cursor: *Cursor, comptime tag: Tag) Token {
    const begin = cursor.pos;
    _ = advance(cursor, 1);
    const span = Span{ .begin = begin, .end = cursor.pos };
    return @unionInit(Token, @tagName(tag), .{ .span = span });
}

fn symbol(intern: *Intern, builtins: Builtins, cursor: *Cursor) !Token {
    const begin = cursor.pos;
    var i: u64 = 0;
    while (i < cursor.source.len and !reserved(cursor.source[i])) : (i += 1) {}
    const contents = advance(cursor, i);
    const end = cursor.pos;
    const span = Span{ .begin = begin, .end = end };
    const interned = try intern.store(contents);
    if (interned.eql(builtins.fn_)) return Token{ .fn_ = .{ .span = span } };
    if (interned.eql(builtins.if_)) return Token{ .if_ = .{ .span = span } };
    if (interned.eql(builtins.else_)) return Token{ .else_ = .{ .span = span } };
    if (interned.eql(builtins.true_)) return Token{ .bool = .{ .value = true, .span = span } };
    if (interned.eql(builtins.false_)) return Token{ .bool = .{ .value = false, .span = span } };
    if (interned.eql(builtins.or_)) return Token{ .or_ = .{ .span = span } };
    if (interned.eql(builtins.mut)) return Token{ .mut = .{ .span = span } };
    if (interned.eql(builtins.undefined)) return Token{ .undefined = .{ .span = span } };
    return Token{ .symbol = .{ .value = interned, .span = span } };
}

fn newLine(cursor: *Cursor) Token {
    const begin = cursor.pos;
    var i: u64 = 0;
    while (i < cursor.source.len and cursor.source[i] == '\n') : (i += 1) {}
    cursor.pos.line += i;
    cursor.pos.column = 1;
    cursor.source = cursor.source[i..];
    return Token{ .new_line = .{ .span = .{ .begin = begin, .end = cursor.pos } } };
}

fn either(cursor: *Cursor, comptime tag: Tag, comptime char: u8, comptime other: Tag) Token {
    const begin = cursor.pos;
    if (cursor.source.len > 1 and cursor.source[1] == char) {
        _ = advance(cursor, 2);
        const span = Span{ .begin = begin, .end = cursor.pos };
        return @unionInit(Token, @tagName(other), .{ .span = span });
    }
    _ = advance(cursor, 1);
    const span = Span{ .begin = begin, .end = cursor.pos };
    return @unionInit(Token, @tagName(tag), .{ .span = span });
}

fn nextToken(cursor: *Cursor, intern: *Intern, builtins: Builtins) !?Token {
    trim(cursor);
    if (cursor.source.len == 0) return null;
    return switch (cursor.source[0]) {
        '0'...'9', '-', '.' => try number(intern, cursor),
        '"' => try string(intern, cursor),
        '=' => either(cursor, .equal, '=', .equal_equal),
        ':' => exact(cursor, .colon),
        '+' => exact(cursor, .plus),
        '*' => exact(cursor, .times),
        '/' => exact(cursor, .slash),
        '^' => exact(cursor, .caret),
        '>' => exact(cursor, .greater),
        '<' => exact(cursor, .less),
        '%' => exact(cursor, .percent),
        '(' => exact(cursor, .left_paren),
        ')' => exact(cursor, .right_paren),
        '{' => exact(cursor, .left_brace),
        '}' => exact(cursor, .right_brace),
        '[' => exact(cursor, .left_bracket),
        ']' => exact(cursor, .right_bracket),
        ',' => exact(cursor, .comma),
        '\n' => newLine(cursor),
        else => try symbol(intern, builtins, cursor),
    };
}

pub fn tokenize(allocator: Allocator, intern: *Intern, compile_errors: *CompileErrors, builtins: Builtins, source: []const u8) !Tokens {
    var cursor = Cursor{
        .source = source,
        .pos = .{ .line = 1, .column = 1 },
    };
    var tokens = List(Token).init(allocator);
    while (try nextToken(&cursor, intern, builtins)) |t| try tokens.append(t);
    return Tokens{
        .tokens = try tokens.toOwnedSlice(),
        .index = 0,
        .intern = intern,
        .compile_errors = compile_errors,
    };
}
