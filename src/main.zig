const std = @import("std");
const Allocator = std.mem.Allocator;
const atom = @import("atom");

const List = std.ArrayList;

fn compileToWat(allocator: Allocator, file_name: []const u8) ![]const u8 {
    const source = try std.fs.cwd().readFileAlloc(allocator, file_name, std.math.maxInt(usize));
    var intern = atom.interner.Intern.init(allocator);
    const builtins = try atom.Builtins.init(&intern);
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    const untyped_module = try atom.parser.parse(allocator, tokens);
    var module = try atom.type_checker.infer.module(allocator, builtins, untyped_module);
    var constraints = atom.type_checker.types.Constraints{
        .equal = List(atom.type_checker.types.Equal).init(allocator),
    };
    const start = try atom.interner.store(&intern, "start");
    var next_type_var: atom.type_checker.types.TypeVar = 0;
    try atom.type_checker.infer.infer(allocator, &constraints, &module, builtins, &next_type_var, start);
    const substitution = try atom.type_checker.solve(allocator, constraints);
    const typed_module = try atom.type_checker.apply(allocator, substitution, module);
    var ir = try atom.lower.buildIr(allocator, builtins, typed_module);
    const alias = try atom.interner.store(&intern, "_start");
    const exports = try allocator.alloc(atom.lower.types.Export, ir.exports.len + 1);
    std.mem.copy(atom.lower.types.Export, exports, ir.exports);
    exports[ir.exports.len] = atom.lower.types.Export{
        .function = .{ .name = start, .alias = alias },
    };
    ir.exports = exports;
    return try atom.codegen.wat(allocator, intern, ir);
}

fn saveWatToFile(file_name: []const u8, wat: []const u8) !void {
    const file = try std.fs.cwd().createFile(file_name, .{});
    try file.writer().writeAll(wat);
}

fn wat2wasm(allocator: Allocator, writer: std.fs.File.Writer, file_name: []const u8) !void {
    const result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &.{ "wat2wasm", file_name },
    });

    if (result.stdout.len > 0) {
        try writer.print("\nstdout: {s}\n", .{result.stdout});
    }
    if (result.stderr.len > 0) {
        try writer.print("\nstderr: {s}\n", .{result.stderr});
    }
}

fn wasmtime(allocator: Allocator, writer: std.fs.File.Writer, file_name: []const u8) !void {
    const result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &.{ "wasmtime", file_name },
    });

    if (result.stdout.len > 0) {
        try writer.print("\nstdout: {s}", .{result.stdout});
    }
    if (result.stderr.len > 0) {
        try writer.print("\nstderr: {s}", .{result.stderr});
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    try writer.writeAll("---- ATOM COMPILER --------------------");

    if (std.os.argv.len < 2) {
        std.debug.panic(
            \\
            \\ERROR - No input file specified
            \\
            \\Correct usage:
            \\
            \\atom <input file>.atom
            \\this will compile and output to <input file>.wat and use wat2wasm to compile to <input file>.wasm
            \\
            \\atom <input file>.atom --execute
            \\this will compile and output to <input file>.wat and use wat2wasm to compile to <input file>.wasm
            \\then execute the wasm file using wasmtime
        , .{});
    }

    const file_name = std.mem.span(std.os.argv[1]);
    const wat = try compileToWat(allocator, file_name);
    const file_name_no_suffix = file_name[0 .. file_name.len - 5];
    const file_name_wat = try std.fmt.allocPrint(allocator, "{s}.wat", .{file_name_no_suffix});
    try saveWatToFile(file_name_wat, wat);
    try wat2wasm(allocator, writer, file_name_wat);

    if (std.os.argv.len == 3) {
        const argument = std.mem.span(std.os.argv[2]);
        if (std.mem.eql(u8, argument, "--execute")) {
            const file_name_wasm = try std.fmt.allocPrint(allocator, "{s}.wasm", .{file_name_no_suffix});
            try wasmtime(allocator, writer, file_name_wasm);
        }
    }
}
