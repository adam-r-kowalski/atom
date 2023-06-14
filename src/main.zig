const std = @import("std");
const wasmer = @cImport(@cInclude("wasmer.h"));
const Allocator = std.mem.Allocator;
const mantis = @import("mantis");

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
                \\mantis <input file>.mantis
                \\this will compile and run the mantis program using the wasmer runtime
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

fn writeWat(allocator: Allocator, flags: Flags, wat_string: []const u8) !void {
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
    void,

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .i32 => |i| try writer.print("{}", .{i}),
            .i64 => |i| try writer.print("{}", .{i}),
            .f32 => |f| try writer.print("{}", .{f}),
            .f64 => |f| try writer.print("{}", .{f}),
            .void => {},
        }
    }
};

const WasmModule = struct {
    allocator: Allocator,
    ast: mantis.type_checker.types.Module,
    engine: *wasmer.wasm_engine_t,
    store: *wasmer.wasm_store_t,
    module: *wasmer.wasm_module_t,
    instance: *wasmer.wasm_instance_t,
    exports: wasmer.wasm_extern_vec_t,

    fn init(allocator: Allocator, ast: mantis.type_checker.types.Module, wat_string: []const u8) WasmModule {
        var wat: wasmer.wasm_byte_vec_t = undefined;
        wasmer.wasm_byte_vec_new(&wat, wat_string.len, wat_string.ptr);
        var wasm_bytes: wasmer.wasm_byte_vec_t = undefined;
        wasmer.wat2wasm(&wat, &wasm_bytes);
        const engine = wasmer.wasm_engine_new();
        const store = wasmer.wasm_store_new(engine);
        const module = wasmer.wasm_module_new(store, &wasm_bytes);
        if (module == null) std.debug.panic("\nError compiling module!\n", .{});
        const config = wasmer.wasi_config_new("example_program");
        const wasi_env = wasmer.wasi_env_new(store, config);
        if (wasi_env == null) std.debug.panic("\nError building WASI env!\n", .{});
        var imports: wasmer.wasm_extern_vec_t = undefined;
        _ = wasmer.wasi_get_imports(store, wasi_env, module, &imports);
        const instance = wasmer.wasm_instance_new(store, module, &imports, null);
        if (instance == null) std.debug.panic("\nError instantiating module!\n", .{});
        if (!wasmer.wasi_env_initialize_instance(wasi_env, store, instance))
            std.debug.panic("\nError initializing WASI instance!\n", .{});
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

    fn run(self: WasmModule, name: mantis.interner.Interned) !Value {
        const func = wasmer.wasi_get_start_function(self.instance);
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
        wasmer.wasm_val_vec_new(&results, results_val.items.len, results_val.items.ptr);
        if (wasmer.wasm_func_call(func, &args, &results)) |_| {
            std.debug.panic("\nError calling start!\n", .{});
        }
        switch (return_type) {
            .i32 => return .{ .i32 = results.data[0].of.i32 },
            .i64 => return .{ .i64 = results.data[0].of.i64 },
            .f32 => return .{ .f32 = results.data[0].of.f32 },
            .f64 => return .{ .f64 = results.data[0].of.f64 },
            .void => return .void,
            else => |k| std.debug.panic("\nUnsupported return type {}!\n", .{k}),
        }
    }
};

fn compileAndRun(allocator: Allocator, intern: *mantis.interner.Intern, errors: *mantis.error_reporter.types.Errors, flags: Flags, source: []const u8) !void {
    const builtins = try mantis.Builtins.init(intern);
    const tokens = try mantis.tokenizer.tokenize(allocator, intern, builtins, source);
    const untyped_ast = try mantis.parser.parse(allocator, tokens);
    var constraints = mantis.type_checker.types.Constraints{
        .equal = List(mantis.type_checker.types.EqualConstraint).init(allocator),
        .next_type_var = .{ .value = 0 },
    };
    var ast = try mantis.type_checker.infer.module(allocator, &constraints, builtins, untyped_ast);
    const export_count = ast.foreign_exports.len;
    const start = try intern.store("start");
    if (export_count == 0) ast.foreign_exports = &.{start};
    for (ast.foreign_exports) |foreign_export| try mantis.type_checker.infer.topLevel(&ast, foreign_export, errors);
    const substitution = try mantis.type_checker.solve_constraints.constraints(allocator, constraints, errors);
    mantis.type_checker.apply_substitution.module(substitution, &ast);
    var ir = try mantis.code_generator.lower.module(allocator, builtins, ast, intern);
    if (export_count == 0) {
        const alias = try intern.store("_start");
        ir.foreign_exports = &.{.{ .name = start, .alias = alias }};
    }
    const wat_string = try std.fmt.allocPrint(allocator, "{}", .{ir});
    if (flags.contains("--wat")) try writeWat(allocator, flags, wat_string);
    if (export_count > 0) {
        try writeWat(allocator, flags, wat_string);
        return;
    }
    const wasm_module = WasmModule.init(allocator, ast, wat_string);
    const value = try wasm_module.run(start);
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    try writer.print("{}", .{value});
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const flags = try Flags.init(allocator);
    const source = try std.fs.cwd().readFileAlloc(allocator, flags.file_name, std.math.maxInt(usize));
    var intern = mantis.interner.Intern.init(allocator);
    var errors = mantis.error_reporter.types.Errors{
        .allocator = arena.allocator(),
        .undefined_variables = List(mantis.error_reporter.types.UndefinedVariable).init(arena.allocator()),
        .type_mismatches = List(mantis.error_reporter.types.TypeMismatch).init(arena.allocator()),
        .source = source,
    };
    compileAndRun(allocator, &intern, &errors, flags, source) catch |e| switch (e) {
        error.CompileError => {
            const stderr = std.io.getStdErr();
            const writer = stderr.writer();
            try writer.print("{}", .{errors});
        },
        else => return e,
    };
}
