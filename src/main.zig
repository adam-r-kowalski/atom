const std = @import("std");
const wasmer = @cImport(@cInclude("wasmer.h"));
const Allocator = std.mem.Allocator;
const neuron = @import("neuron");

const List = std.ArrayList;

const Flags = struct {
    file_name: []const u8,
    map: std.StringHashMap(void),

    fn init(allocator: Allocator) !Flags {
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
        var map = std.StringHashMap(void).init(allocator);
        for (std.os.argv[2..]) |flag| try map.putNoClobber(std.mem.span(flag), void{});
        return Flags{ .file_name = file_name, .map = map };
    }

    pub fn contains(self: Flags, flag: []const u8) bool {
        return self.map.contains(flag);
    }
};

fn printTime(writer: std.fs.File.Writer, label: []const u8, time: u64) !void {
    try writer.print("\n{s}: {d:0.07}s", .{
        label,
        @intToFloat(f64, time) / std.time.ns_per_s,
    });
}

fn writeWat(allocator: Allocator, flags: Flags, wat_string: []const u8) !void {
    if (!flags.contains("--wat")) return;
    const file_name_no_suffix = flags.file_name[0 .. flags.file_name.len - 7];
    const file_name_wat = try std.fmt.allocPrint(allocator, "{s}.wat", .{file_name_no_suffix});
    const file = try std.fs.cwd().createFile(file_name_wat, .{});
    try file.writer().writeAll(wat_string);
}

const Value = union(enum) {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .i32 => |i| try writer.print("{}", .{i}),
            .i64 => |i| try writer.print("{}", .{i}),
            .f32 => |f| try writer.print("{}", .{f}),
            .f64 => |f| try writer.print("{}", .{f}),
        }
    }
};

const WasmModule = struct {
    allocator: Allocator,
    ast: neuron.Module,
    engine: *wasmer.wasm_engine_t,
    store: *wasmer.wasm_store_t,
    module: *wasmer.wasm_module_t,
    instance: *wasmer.wasm_instance_t,
    exports: wasmer.wasm_extern_vec_t,

    fn init(allocator: Allocator, ast: neuron.Module, wat_string: []const u8) WasmModule {
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
        return WasmModule{
            .allocator = allocator,
            .ast = ast,
            .engine = engine.?,
            .store = store.?,
            .module = module.?,
            .instance = instance.?,
            .exports = wasm_exports,
        };
    }

    fn run(self: WasmModule, name: neuron.Interned) !Value {
        const func = wasmer.wasm_extern_as_func(self.exports.data[0]);
        if (func == null) std.debug.panic("\nError getting start!\n", .{});
        var args_val = [0]wasmer.wasm_val_t{};
        var results_val = List(wasmer.wasm_val_t).init(self.allocator);
        const exported_define = self.ast.typed.get(name).?.define;
        const exported_function = exported_define.value.function;
        if (exported_function.parameters.len != 0)
            std.debug.panic("\nOnly functions with no parameters supported!\n", .{});
        const return_type = exported_function.return_type;
        if (return_type != .void) {
            const value = wasmer.wasm_val_t{ .kind = wasmer.WASM_ANYREF, .of = .{ .ref = null } };
            try results_val.append(value);
        }
        var args: wasmer.wasm_val_vec_t = undefined;
        var results: wasmer.wasm_val_vec_t = undefined;
        wasmer.wasm_val_vec_new(&args, 0, &args_val);
        wasmer.wasm_val_vec_new(&results, 1, results_val.items.ptr);
        if (wasmer.wasm_func_call(func, &args, &results)) |_| {
            std.debug.panic("\nError calling start!\n", .{});
        }
        switch (return_type) {
            .i32 => return .{ .i32 = results.data[0].of.i32 },
            .i64 => return .{ .i64 = results.data[0].of.i64 },
            .f32 => return .{ .f32 = results.data[0].of.f32 },
            .f64 => return .{ .f64 = results.data[0].of.f64 },
            else => |k| std.debug.panic("\nUnsupported return type {}!\n", .{k}),
        }
    }
};

pub fn main() !void {
    var timer = try std.time.Timer.start();
    const t0 = timer.read();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    const flags = try Flags.init(allocator);
    const t1 = timer.read();
    const source = try std.fs.cwd().readFileAlloc(allocator, flags.file_name, std.math.maxInt(usize));
    const t2 = timer.read();
    var intern = neuron.Intern.init(allocator);
    const builtins = try neuron.Builtins.init(&intern);
    const t3 = timer.read();
    var tokens = try neuron.tokenize(allocator, &intern, builtins, source);
    const t4 = timer.read();
    const untyped_ast = try neuron.parse(allocator, &tokens);
    const t5 = timer.read();
    var constraints = neuron.Constraints.init(arena.allocator());
    var next_type_var: neuron.TypeVar = 0;
    var ast = try neuron.Module.init(arena.allocator(), &constraints, &next_type_var, builtins, untyped_ast);
    const start = try intern.store("start");
    try neuron.type_checker.infer(&ast, start);
    const substitution = try constraints.solve(allocator);
    ast.apply(substitution);
    const t6 = timer.read();
    var ir = try neuron.lower.buildIr(allocator, builtins, ast);
    const alias = try intern.store("_start");
    ir.exports = &.{.{ .name = start, .alias = alias }};
    const t7 = timer.read();
    const wat_string = try std.fmt.allocPrint(allocator, "{}", .{ir});
    const t8 = timer.read();
    try writeWat(allocator, flags, wat_string);
    const t9 = timer.read();
    const wasm_module = WasmModule.init(allocator, ast, wat_string);
    const t10 = timer.read();
    const value = try wasm_module.run(start);
    const t11 = timer.read();
    try writer.print("{}", .{value});
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
