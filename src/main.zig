const std = @import("std");
const Allocator = std.mem.Allocator;

const builtin = @import("./builtin.zig");
const builtin_add = builtin.builtin_add;
const builtin_car = builtin.builtin_car;
const builtin_cdr = builtin.builtin_cdr;
const builtin_cons = builtin.builtin_cons;
const builtin_divide = builtin.builtin_divide;
const builtin_eq = builtin.builtin_eq;
const builtin_less = builtin.builtin_less;
const builtin_multiply = builtin.builtin_multiply;
const builtin_numeq = builtin.builtin_numeq;
const builtin_pairp = builtin.builtin_pairp;
const builtin_subtract = builtin.builtin_subtract;

const data = @import("./data.zig");
const deinit = data.deinit;
const gc = data.gc;
const gc_mark = data.gc_mark;
const make_builtin = data.make_builtin;
const make_sym = data.make_sym;

const eval = @import("./eval.zig");
const env_create = eval.env_create;
const env_get = eval.env_get;
const env_set = eval.env_set;
const eval_expr = eval.eval_expr;

const lisp = @import("./lisp.zig");
const Atom = lisp.Atom;
const Error = lisp.Error;
const nil = lisp.nil;

const print_expr = @import("./print.zig").print_expr;
const read_expr = @import("./read.zig").read_expr;

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const a = gpa.allocator();
    // var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    // defer arena.deinit();
    // const a = arena.allocator();

    defer deinit(a);

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    const input_buffer_length = 256;
    var input_buffer: [input_buffer_length]u8 = undefined;

    var env = try env_create(a, nil);

    try env_set(a, env, try make_sym(a, "CAR"), make_builtin(builtin_car));
    try env_set(a, env, try make_sym(a, "CDR"), make_builtin(builtin_cdr));
    try env_set(a, env, try make_sym(a, "CONS"), make_builtin(builtin_cons));

    try env_set(a, env, try make_sym(a, "+"), make_builtin(builtin_add));
    try env_set(a, env, try make_sym(a, "-"), make_builtin(builtin_subtract));
    try env_set(a, env, try make_sym(a, "*"), make_builtin(builtin_multiply));
    try env_set(a, env, try make_sym(a, "/"), make_builtin(builtin_divide));

    try env_set(a, env, try make_sym(a, "T"), try make_sym(a, "T"));
    try env_set(a, env, try make_sym(a, "="), make_builtin(builtin_numeq));
    try env_set(a, env, try make_sym(a, "<"), make_builtin(builtin_less));

    try env_set(a, env, try make_sym(a, "EQ?"), make_builtin(builtin_eq));
    try env_set(a, env, try make_sym(a, "PAIR?"), make_builtin(builtin_pairp));

    try load_file(a, env, "library.lisp");

    while (true) {
        try stdout.writeAll("> ");

        const input = (try stdin.readUntilDelimiterOrEof(&input_buffer, '\n')) orelse {
            // reached input end-of-file
            break;
        };
        var end = input;
        const expr = read_expr(a, input, &end) catch |err| {
            try handleError(err, null, input);
            continue;
        };
        const result = eval_expr(a, expr, env) catch |err| {
            try handleError(err, expr, null);
            continue;
        };
        try print_expr(result);
        try stdout.writeByte('\n');
    }
}

fn slurp(a: Allocator, path: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const len = @as(usize, (try file.stat()).size);
    const buf = try file.reader().readAllAlloc(a, len);
    return buf;
}

fn load_file(a: Allocator, env: Atom, path: []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Reading {s}...\n", .{path});
    const text = try slurp(a, path);
    defer a.free(text);
    var p = text;
    var end = text;
    while (read_expr(a, p, &end)) |expr| : (p = end) {
        if (eval_expr(a, expr, env)) |result| {
            try print_expr(result);
            try stdout.writeByte('\n');
        } else |_| {
            try stdout.print("Error in expression:\n\t", .{});
            try print_expr(expr);
            try stdout.writeByte('\n');
        }
    } else |_| {}
}

fn handleError(err: Error, expr: ?Atom, input: ?[]const u8) !void {
    const message = switch (err) {
        error.Syntax => "Syntax error",
        error.Unbound => "Symbol not bound",
        error.Args => "Wrong number of arguments",
        error.Type => "Wrong type",
        error.OutOfMemory => "Out of memory!",
    };
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Error: {s}\n", .{message});
    if (expr) |exp| {
        try stdout.writeAll("Evaluating: ");
        try print_expr(exp);
    }
    if (input) |str| {
        try stdout.print("Reading: {s}", .{str});
    }
    try stdout.writeByte('\n');
}
