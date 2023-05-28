const std = @import("std");
const wasmer = @cImport(@cInclude("wasmer.h"));
const Allocator = std.mem.Allocator;
const neuron = @import("neuron");

const List = std.ArrayList;
const Flags = std.StringHashMap(void);

fn printTime(writer: std.fs.File.Writer, label: []const u8, time: u64) !void {
    try writer.print("\n{s}: {d:0.07}s", .{
        label,
        @intToFloat(f64, time) / std.time.ns_per_s,
    });
}

pub fn main() !void {
    var timer = try std.time.Timer.start();
    const t0 = timer.read();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    if (std.os.argv.len < 2) {
        std.debug.panic(
            \\---- ERROR - No input file specified --------------------
            \\
            \\Correct usage:
            \\
            \\neuron <input file>.neuron
            \\this will compile and run the neuron program using the wasmer runtime
        , .{});
    }

    const file_name = std.mem.span(std.os.argv[1]);
    var flags = Flags.init(allocator);
    for (std.os.argv[2..]) |flag| try flags.putNoClobber(std.mem.span(flag), void{});
    const t1 = timer.read();
    const source = try std.fs.cwd().readFileAlloc(allocator, file_name, std.math.maxInt(usize));
    const t2 = timer.read();
    var intern = neuron.interner.Intern.init(allocator);
    const builtins = try neuron.Builtins.init(&intern);
    const t3 = timer.read();
    var tokens = try neuron.tokenizer.tokenize(allocator, &intern, builtins, source);
    const t4 = timer.read();
    const untyped_ast = try neuron.parser.parse(allocator, &tokens);
    const t5 = timer.read();
    var constraints = neuron.type_checker.types.Constraints.init(arena.allocator());
    var next_type_var: neuron.type_checker.types.TypeVar = 0;
    var ast = try neuron.type_checker.types.Ast.init(arena.allocator(), &constraints, &next_type_var, builtins, untyped_ast);
    try ast.infer("start");
    const substitution = try constraints.solve(allocator);
    ast.apply(substitution);
    const t6 = timer.read();
    var ir = try neuron.lower.buildIr(allocator, builtins, ast);
    const start = try intern.store("start");
    const alias = try intern.store("_start");
    const exports = try allocator.alloc(neuron.lower.types.Export, ir.exports.len + 1);
    std.mem.copy(neuron.lower.types.Export, exports, ir.exports);
    exports[ir.exports.len] = neuron.lower.types.Export{ .name = start, .alias = alias };
    ir.exports = exports;
    const t7 = timer.read();
    const wat_string = try neuron.codegen.wat(allocator, intern, ir);
    const t8 = timer.read();
    if (flags.contains("--wat")) {
        const file_name_no_suffix = file_name[0 .. file_name.len - 7];
        const file_name_wat = try std.fmt.allocPrint(allocator, "{s}.wat", .{file_name_no_suffix});
        const file = try std.fs.cwd().createFile(file_name_wat, .{});
        try file.writer().writeAll(wat_string);
    }
    const t9 = timer.read();
    var wat: wasmer.wasm_byte_vec_t = undefined;
    wasmer.wasm_byte_vec_new(&wat, wat_string.len, wat_string.ptr);
    var wasm_bytes: wasmer.wasm_byte_vec_t = undefined;
    wasmer.wat2wasm(&wat, &wasm_bytes);
    const engine = wasmer.wasm_engine_new();
    const store = wasmer.wasm_store_new(engine);
    const module = wasmer.wasm_module_new(store, &wasm_bytes);
    if (module == null) std.debug.panic("\nError compiling module!\n", .{});
    const imports: wasmer.wasm_extern_vec_t = undefined;
    const instance = wasmer.wasm_instance_new(store, module, &imports, null);
    if (instance == null) std.debug.panic("\nError instantiating module!\n", .{});
    var wasm_exports: wasmer.wasm_extern_vec_t = undefined;
    wasmer.wasm_instance_exports(instance, &wasm_exports);
    if (wasm_exports.size == 0) std.debug.panic("\nError getting exports!\n", .{});
    if (exports.len != 1 or wasm_exports.size != exports.len) std.debug.panic("\nOnly one export supported!\n", .{});
    const start_func = wasmer.wasm_extern_as_func(wasm_exports.data[0]);
    if (start_func == null) std.debug.panic("\nError getting start!\n", .{});
    var args_val = [0]wasmer.wasm_val_t{};
    var results_val = List(wasmer.wasm_val_t).init(allocator);
    const exported_define = ast.typed.get(start).?.define;
    const exported_function = exported_define.value.function;
    if (exported_function.parameters.len != 0)
        std.debug.panic("\nOnly functions with no parameters supported!\n", .{});
    const return_type = exported_function.return_type;
    if (return_type != .void) {
        try results_val.append(wasmer.wasm_val_t{
            .kind = wasmer.WASM_ANYREF,
            .of = .{ .ref = null },
        });
    }
    var args: wasmer.wasm_val_vec_t = undefined;
    var results: wasmer.wasm_val_vec_t = undefined;
    wasmer.wasm_val_vec_new(&args, 0, &args_val);
    wasmer.wasm_val_vec_new(&results, 1, results_val.items.ptr);
    const t10 = timer.read();
    if (wasmer.wasm_func_call(start_func, &args, &results)) |_| {
        std.debug.panic("\nError calling start!\n", .{});
    }
    const t11 = timer.read();
    switch (return_type) {
        .i32 => try writer.print("{}", .{results.data[0].of.i32}),
        .i64 => try writer.print("{}", .{results.data[0].of.i64}),
        .f32 => try writer.print("{}", .{results.data[0].of.f32}),
        .f64 => try writer.print("{}", .{results.data[0].of.f64}),
        else => {},
    }
    if (flags.contains("--timings")) {
        try printTime(writer, "total", t11 - t0);
        try printTime(writer, "read file", t2 - t1);
        try printTime(writer, "tokenize", t4 - t3);
        try printTime(writer, "parse", t5 - t4);
        try printTime(writer, "type infer", t6 - t5);
        try printTime(writer, "ir", t7 - t6);
        try printTime(writer, "codegen", t8 - t7);
        try printTime(writer, "write wat", t9 - t8);
        try printTime(writer, "init wasmer", t10 - t9);
        try printTime(writer, "execute", t11 - t10);
    }
}
