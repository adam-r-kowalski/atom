const std = @import("std");
const wasmer = @cImport(@cInclude("wasmer.h"));
const Allocator = std.mem.Allocator;
const atom = @import("atom");

const List = std.ArrayList;

const Timings = struct {
    read_file: u64,
    tokenize: u64,
    parse: u64,
    build_module: u64,
    infer: u64,
    solve: u64,
    apply: u64,
    build_ir: u64,
    codegen: u64,
    save_wat_to_file: u64,
    wat2wasm: u64,
    wasmtime: u64,
    total: u64,
};

fn compileToWat(timer: *std.time.Timer, timings: *Timings, allocator: Allocator, file_name: []const u8) ![]const u8 {
    const t0 = timer.read();
    const source = try std.fs.cwd().readFileAlloc(allocator, file_name, std.math.maxInt(usize));
    const t1 = timer.read();
    var intern = atom.interner.Intern.init(allocator);
    const builtins = try atom.Builtins.init(&intern);
    const t2 = timer.read();
    const tokens = try atom.tokenizer.tokenize(allocator, &intern, builtins, source);
    const t3 = timer.read();
    const untyped_module = try atom.parser.parse(allocator, tokens);
    const t4 = timer.read();
    var module = try atom.type_checker.infer.module(allocator, builtins, untyped_module);
    const t5 = timer.read();
    var constraints = atom.type_checker.types.Constraints{
        .equal = List(atom.type_checker.types.Equal).init(allocator),
    };
    const start = try atom.interner.store(&intern, "start");
    var next_type_var: atom.type_checker.types.TypeVar = 0;
    const t6 = timer.read();
    try atom.type_checker.infer.infer(allocator, &constraints, &module, builtins, &next_type_var, start);
    const t7 = timer.read();
    const substitution = try atom.type_checker.solve(allocator, constraints);
    const t8 = timer.read();
    const typed_module = try atom.type_checker.apply(allocator, substitution, module);
    const t9 = timer.read();
    var ir = try atom.lower.buildIr(allocator, builtins, typed_module);
    const t10 = timer.read();
    const alias = try atom.interner.store(&intern, "_start");
    const exports = try allocator.alloc(atom.lower.types.Export, ir.exports.len + 1);
    std.mem.copy(atom.lower.types.Export, exports, ir.exports);
    exports[ir.exports.len] = atom.lower.types.Export{ .name = start, .alias = alias };
    ir.exports = exports;
    const t11 = timer.read();
    const wat = try atom.codegen.wat(allocator, intern, ir);
    const t12 = timer.read();
    timings.read_file = t1 - t0;
    timings.tokenize = t3 - t2;
    timings.parse = t4 - t3;
    timings.build_module = t5 - t4;
    timings.infer = t7 - t6;
    timings.solve = t8 - t7;
    timings.apply = t9 - t8;
    timings.build_ir = t10 - t9;
    timings.codegen = t12 - t11;
    return wat;
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

fn printTime(writer: std.fs.File.Writer, label: []const u8, time: u64) !void {
    try writer.print("\n{s}: {d:0.07}s", .{
        label,
        @intToFloat(f64, time) / std.time.ns_per_s,
    });
}

fn printTimings(writer: std.fs.File.Writer, timings: Timings) !void {
    try printTime(writer, "read_file", timings.read_file);
    try printTime(writer, "tokenize", timings.tokenize);
    try printTime(writer, "parse", timings.parse);
    try printTime(writer, "build_module", timings.build_module);
    try printTime(writer, "infer", timings.infer);
    try printTime(writer, "solve", timings.solve);
    try printTime(writer, "apply", timings.apply);
    try printTime(writer, "build_ir", timings.build_ir);
    try printTime(writer, "codegen", timings.codegen);
    try printTime(writer, "save_wat_to_file", timings.save_wat_to_file);
    try printTime(writer, "wat2wasm", timings.wat2wasm);
    try printTime(writer, "wasmtime", timings.wasmtime);
    try printTime(writer, "total", timings.total);
}

pub fn main() !void {
    var timings: Timings = undefined;
    var timer = try std.time.Timer.start();
    const t0 = timer.read();
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
    const wat_string = try compileToWat(&timer, &timings, allocator, file_name);

    const file_name_no_suffix = file_name[0 .. file_name.len - 5];
    const file_name_wat = try std.fmt.allocPrint(allocator, "{s}.wat", .{file_name_no_suffix});
    const t1 = timer.read();
    try saveWatToFile(file_name_wat, wat_string);
    const t2 = timer.read();

    // try wat2wasm(allocator, writer, file_name_wat);
    var wat: wasmer.wasm_byte_vec_t = undefined;
    wasmer.wasm_byte_vec_new(&wat, wat_string.len, wat_string.ptr);

    var wasm_bytes: wasmer.wasm_byte_vec_t = undefined;
    wasmer.wat2wasm(&wat, &wasm_bytes);

    const engine = wasmer.wasm_engine_new();
    const store = wasmer.wasm_store_new(engine);
    const module = wasmer.wasm_module_new(store, &wasm_bytes);
    if (module == null) std.debug.panic("\nError compiling module!\n", .{});

    const t3 = timer.read();
    const imports: wasmer.wasm_extern_vec_t = undefined;
    const instance = wasmer.wasm_instance_new(store, module, &imports, null);
    if (instance == null) std.debug.panic("\nError instantiating module!\n", .{});

    var exports: wasmer.wasm_extern_vec_t = undefined;
    wasmer.wasm_instance_exports(instance, &exports);
    if (exports.size == 0) std.debug.panic("\nError getting exports!\n", .{});

    const start_func = wasmer.wasm_extern_as_func(exports.data[0]);
    if (start_func == null) std.debug.panic("\nError getting start!\n", .{});

    var args_val = [0]wasmer.wasm_val_t{};
    var results_val = [1]wasmer.wasm_val_t{wasmer.wasm_val_t{
        .kind = wasmer.WASM_I32,
        .of = .{ .i32 = 5 },
    }};
    var args: wasmer.wasm_val_vec_t = undefined;
    var results: wasmer.wasm_val_vec_t = undefined;
    wasmer.wasm_val_vec_new(&args, 0, &args_val);
    wasmer.wasm_val_vec_new(&results, 1, &results_val);
    if (wasmer.wasm_func_call(start_func, &args, &results)) |_| {
        std.debug.panic("\nError calling start!\n", .{});
    }

    try writer.print("\n{}\n", .{results.data[0].of.i32});

    if (std.os.argv.len == 3) {
        const argument = std.mem.span(std.os.argv[2]);
        if (std.mem.eql(u8, argument, "--execute")) {
            const file_name_wasm = try std.fmt.allocPrint(allocator, "{s}.wasm", .{file_name_no_suffix});
            try wasmtime(allocator, writer, file_name_wasm);
        }
    }
    const t4 = timer.read();
    timings.save_wat_to_file = t2 - t1;
    timings.wat2wasm = t3 - t2;
    timings.wasmtime = t4 - t3;
    timings.total = t4 - t0;
    try printTimings(writer, timings);
}
