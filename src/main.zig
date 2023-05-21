const std = @import("std");
const atom = @import("atom");

const List = std.ArrayList;

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
            \\Correct usage: atom <input file>
        , .{});
    }

    const file_name = std.mem.span(std.os.argv[1]);
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
    var ir = try atom.lower.buildIr(allocator, typed_module);

    const alias = try atom.interner.store(&intern, "_start");
    const exports = try allocator.alloc(atom.lower.types.Export, ir.exports.len + 1);
    std.mem.copy(atom.lower.types.Export, exports, ir.exports);
    exports[ir.exports.len] = atom.lower.types.Export{
        .function = .{ .name = start, .alias = alias },
    };
    ir.exports = exports;

    const wat = try atom.codegen.wat(allocator, intern, ir);

    const file_name_no_suffix = file_name[0 .. file_name.len - 5];
    const output_file_name = try std.fmt.allocPrint(allocator, "{s}.wat", .{file_name_no_suffix});
    const file = try std.fs.cwd().createFile(output_file_name, .{});
    try file.writer().writeAll(wat);

    const result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &.{ "wat2wasm", output_file_name },
    });

    if (result.stdout.len > 0) {
        try writer.print("\nstdout: {s}\n", .{result.stdout});
    }
    if (result.stderr.len > 0) {
        try writer.print("\nstderr: {s}\n", .{result.stderr});
    }
}
