const std = @import("std");
const types = @import("types.zig");

pub const Iterator = struct {
    tokens: []const types.Token,
    index: usize,

    pub fn init(tokens: []const types.Token) Iterator {
        return Iterator{ .tokens = tokens, .index = 0 };
    }

    fn consumeComments(self: *Iterator) void {
        const len = self.tokens.len;
        var index = self.index;
        while (index < len and self.tokens[index] == .comment) : (index += 1) {}
        self.index = index;
    }

    pub fn peek(self: *Iterator) ?types.Token {
        self.consumeComments();
        if (self.index >= self.tokens.len) return null;
        return self.tokens[self.index];
    }

    pub fn next(self: *Iterator) ?types.Token {
        self.consumeComments();
        if (self.index >= self.tokens.len) return null;
        const index = self.index;
        self.index += 1;
        return self.tokens[index];
    }

    pub fn advance(self: *Iterator) void {
        self.index += 1;
    }

    pub fn consume(self: *Iterator, tag: std.meta.Tag(types.Token)) types.Token {
        const t = self.next().?;
        if (std.meta.activeTag(t) != tag)
            std.debug.panic("\nExpected token {} found {}", .{ tag, t });
        return t;
    }

    pub fn maybeConsume(self: *Iterator, tag: std.meta.Tag(types.Token)) void {
        if (self.peek()) |t| {
            if (std.meta.activeTag(t) == tag)
                self.advance();
        }
    }

    pub fn consumeNewLines(self: *Iterator) void {
        while (self.peek()) |t| {
            switch (t) {
                .new_line => self.advance(),
                else => return,
            }
        }
    }
};
