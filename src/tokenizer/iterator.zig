const types = @import("types.zig");

pub const Iterator = struct {
    tokens: []const types.Token,
    index: usize,

    pub fn init(tokens: []const types.Token) Iterator {
        return Iterator{ .tokens = tokens, .index = 0 };
    }

    pub fn peek(self: Iterator) ?types.Token {
        if (self.index >= self.tokens.len) return null;
        return self.tokens[self.index];
    }

    pub fn next(self: *Iterator) ?types.Token {
        if (self.index >= self.tokens.len) return null;
        const index = self.index;
        self.index += 1;
        return self.tokens[index];
    }

    pub fn advance(self: *Iterator) void {
        self.index += 1;
    }
};
