const std = @import("std");

const Matrix = struct {
    allocator: std.mem.Allocator,
    rows: usize,
    cols: usize,
    data: []u32,

    fn init(allocator: std.mem.Allocator, rows: usize, cols: usize) !Matrix {
        const data = try allocator.alloc(u32, rows * cols);
        return Matrix{
            .allocator = allocator,
            .rows = rows,
            .cols = cols,
            .data = data,
        };
    }

    fn deinit(self: *Matrix) void {
        self.allocator.free(self.data);
    }

    fn get(self: Matrix, row: usize, col: usize) u32 {
        return self.data[row * self.cols + col];
    }

    fn set(self: *Matrix, row: usize, col: usize, value: u32) void {
        self.data[row * self.cols + col] = value;
    }
};

pub fn editDistance(allocator: std.mem.Allocator, a: []const u8, b: []const u8) !u32 {
    var d = try Matrix.init(allocator, a.len + 1, b.len + 1);
    defer d.deinit();
    for (0..a.len + 1) |i| d.set(i, 0, @intCast(i));
    for (0..b.len + 1) |j| d.set(0, j, @intCast(j));
    for (1..a.len + 1) |i| {
        for (1..b.len + 1) |j| {
            const cost: u32 = if (a[i - 1] == b[j - 1]) 0 else 1;
            const deletion = d.get(i - 1, j) + 1;
            const insertion = d.get(i, j - 1) + 1;
            const substitution = d.get(i - 1, j - 1) + cost;
            d.set(i, j, @min(@min(deletion, insertion), substitution));
            if (i > 1 and j > 1 and a[i - 1] == b[j - 2] and a[i - 2] == b[j - 1]) {
                const transposition = d.get(i - 2, j - 2) + cost;
                d.set(i, j, @min(d.get(i, j), transposition));
            }
        }
    }
    return d.get(a.len, b.len);
}
