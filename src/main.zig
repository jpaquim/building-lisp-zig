const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = union(enum) {
    nil,
    pair: *Pair,
    symbol: []const u8,
    integer: i64,
    builtin: Builtin,
    closure: *Pair,
    macro: *Pair,
};

const Atom = struct {
    value: Value,
};

const Pair = struct {
    atom: [2]Atom,
};

const Builtin = fn (a: Allocator, args: Atom, result: *Atom) Error!void;

fn car(p: Atom) Atom {
    const pair = if (p.value == .pair) p.value.pair else if (p.value == .closure) p.value.closure else p.value.macro;
    return pair.atom[0];
}

fn carP(p: Atom) *Atom {
    const pair = if (p.value == .pair) p.value.pair else if (p.value == .closure) p.value.closure else p.value.macro;
    return &pair.atom[0];
}

fn cdr(p: Atom) Atom {
    const pair = if (p.value == .pair) p.value.pair else if (p.value == .closure) p.value.closure else p.value.macro;
    return pair.atom[1];
}

fn cdrP(p: Atom) *Atom {
    const pair = if (p.value == .pair) p.value.pair else if (p.value == .closure) p.value.closure else p.value.macro;
    return &pair.atom[1];
}

fn nilp(atom: Atom) bool {
    return atom.value == .nil;
}

const nil = Atom{ .value = .nil };

fn cons(a: Allocator, car_val: Atom, cdr_val: Atom) !Atom {
    var pair = try a.create(Pair);
    pair.* = .{ .atom = .{ car_val, cdr_val } };
    return Atom{ .value = .{ .pair = pair } };
}

fn make_int(x: i64) Atom {
    return .{ .value = .{ .integer = x } };
}

var sym_table = nil;

fn make_sym(a: Allocator, s: []const u8) !Atom {
    var atom: Atom = undefined;
    var p = sym_table;
    while (!nilp(p)) : (p = cdr(p)) {
        atom = car(p);
        if (std.mem.eql(u8, atom.value.symbol, s)) {
            return atom;
        }
    }
    atom.value = .{ .symbol = try a.dupe(u8, s) };
    sym_table = try cons(a, atom, sym_table);
    return atom;
}

fn make_builtin(f: Builtin) Atom {
    return .{ .value = .{ .builtin = f } };
}

fn make_closure(a: Allocator, env: Atom, args: Atom, body: Atom) !Atom {
    if (!listp(body)) return error.Syntax;

    var p = args;
    while (!nilp(p)) : (p = cdr(p)) {
        if (p.value == .symbol) break else if (p.value != .pair or car(p).value != .symbol)
            return error.Type;
    }
    const result = try cons(a, env, try cons(a, args, body));
    return Atom{ .value = .{ .closure = result.value.pair } };
}

const PrintError = std.fs.File.WriteError;
fn print_expr(atom: Atom) PrintError!void {
    const stdout = std.io.getStdOut().writer();

    switch (atom.value) {
        .nil => {
            try stdout.writeAll("NIL");
        },
        .pair => {
            try stdout.writeByte('(');
            try print_expr(car(atom));
            var current_atom = cdr(atom);
            while (!nilp(current_atom)) {
                if (current_atom.value == .pair) {
                    try stdout.writeByte(' ');
                    try print_expr(car(current_atom));
                    current_atom = cdr(current_atom);
                } else {
                    try stdout.writeAll(" . ");
                    try print_expr(current_atom);
                    break;
                }
            }
            try stdout.writeByte(')');
        },
        .symbol => |symbol| {
            try stdout.writeAll(symbol);
        },
        .integer => |integer| {
            try stdout.print("{}", .{integer});
        },
        .builtin => |builtin| {
            try stdout.print("#<BUILTIN:0x{x}>", .{@ptrToInt(builtin)});
        },
        .closure => |closure| {
            try stdout.print("#<CLOSURE:0x{x}>", .{@ptrToInt(&closure)});
        },
        .macro => |macro| {
            try stdout.print("#<MACRO:0x{x}>", .{@ptrToInt(&macro)});
        },
    }
}

fn strspn(s: []const u8, accept: []const u8) usize {
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        for (accept) |char| {
            if (s[i] == char) break;
        } else return i;
    }
    return i;
}

fn strcspn(s: []const u8, reject: []const u8) usize {
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        for (reject) |char| {
            if (s[i] == char) return i;
        }
    }
    return i;
}

fn strchr(s: []const u8, c: u8) ?*const u8 {
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        if (s[i] == c) return &s[i];
    } else return null;
}

fn lex(str: []const u8, end_ptr: *[]const u8) ![]const u8 {
    const ws = " \t\n";
    const delim = "() \t\n";
    const prefix = "()'`";

    const start = strspn(str, ws);

    if (start == str.len) return error.Syntax;

    const end = if (strchr(prefix, str[start]) != null)
        start + 1
    else if (str[start] == ',')
        start + @as(usize, if (str[start + 1] == '@') 2 else 1)
    else
        start + strcspn(str[start..], delim);
    end_ptr.* = str[end..];
    return str[start..end];
}

fn parse_simple(a: Allocator, input: []const u8) !Atom {
    if (std.fmt.parseInt(i64, input, 10)) |integer| {
        return make_int(integer);
    } else |_| {}
    const buf = try std.ascii.allocUpperString(a, input);
    defer a.free(buf);
    if (std.mem.eql(u8, buf, "NIL")) {
        return nil;
    } else {
        return make_sym(a, buf);
    }
}

const ReadError = error{
    Syntax,
    OutOfMemory,
};

fn read_list(a: Allocator, input: []const u8, end_ptr: *[]const u8) ReadError!Atom {
    var p = nil;
    var result = nil;

    end_ptr.* = input;

    while (true) {
        var str = end_ptr.*;
        const token = try lex(str, end_ptr);
        if (token[0] == ')') {
            return result;
        }
        if (token[0] == '.' and token.len == 1) {
            if (nilp(p)) return error.Syntax;

            str = end_ptr.*;
            const item = try read_expr(a, str, end_ptr);
            cdrP(p).* = item;

            str = end_ptr.*;
            const next_token = try lex(str, end_ptr);
            if (next_token[0] != ')') return error.Syntax;
            return result;
        }

        const item = try read_expr(a, str, end_ptr);
        if (nilp(p)) {
            result = try cons(a, item, nil);
            p = result;
        } else {
            cdrP(p).* = try cons(a, item, nil);
            p = cdr(p);
        }
    }
}

fn read_expr(a: Allocator, input: []const u8, end_ptr: *[]const u8) ReadError!Atom {
    const token = try lex(input, end_ptr);
    const str = end_ptr.*;
    if (token[0] == '(') {
        return read_list(a, str, end_ptr);
    } else if (token[0] == ')') {
        return error.Syntax;
    } else if (token[0] == '\'') {
        const result = try cons(a, try make_sym(a, "QUOTE"), try cons(a, nil, nil));
        carP(cdr(result)).* = try read_expr(a, str, end_ptr);
        return result;
    } else if (token[0] == '`') {
        const result = try cons(a, try make_sym(a, "QUASIQUOTE"), try cons(a, nil, nil));
        carP(cdr(result)).* = try read_expr(a, str, end_ptr);
        return result;
    } else if (token[0] == ',') {
        const result = try cons(
            a,
            try make_sym(a, if (token.len > 1 and token[1] == '@') "UNQUOTE-SPLICING" else "UNQUOTE"),
            try cons(a, nil, nil),
        );
        carP(cdr(result)).* = try read_expr(a, str, end_ptr);
        return result;
    } else {
        return parse_simple(a, token);
    }
}

fn env_create(a: Allocator, parent: Atom) !Atom {
    return cons(a, parent, nil);
}

fn env_get(env: Atom, symbol: Atom) error{Unbound}!Atom {
    const parent = car(env);
    var bs = cdr(env);
    while (!nilp(bs)) {
        const b = car(bs);
        if (sliceEql(car(b).value.symbol, symbol.value.symbol)) {
            return cdr(b);
        }
        bs = cdr(bs);
    }
    if (nilp(parent)) return error.Unbound;
    return env_get(parent, symbol);
}

fn sliceEql(a: []const u8, b: []const u8) bool {
    return a.ptr == b.ptr and a.len == b.len;
}

fn env_set(a: Allocator, env: Atom, symbol: Atom, value: Atom) !void {
    var bs = cdr(env);
    var b = nil;
    while (!nilp(bs)) {
        b = car(bs);
        const car_b = car(b);
        if (sliceEql(car_b.value.symbol, symbol.value.symbol)) {
            cdrP(b).* = value;
            return;
        }
        bs = cdr(bs);
    }
    b = try cons(a, symbol, value);
    cdrP(env).* = try cons(a, b, cdr(env));
}

fn listp(expr: Atom) bool {
    var it = expr;
    while (!nilp(it)) : (it = cdr(it)) {
        if (it.value != .pair) return false;
    } else return true;
}

fn copy_list(a: Allocator, list: Atom) !Atom {
    if (nilp(list)) return nil;

    const result = try cons(a, car(list), nil);
    var p = result;
    var it = cdr(list);
    while (!nilp(it)) : (it = cdr(it)) {
        cdrP(p).* = try cons(a, car(it), nil);
        p = cdr(p);
    }
    return result;
}

fn list_get(list: Atom, k: usize) Atom {
    var it = list;
    var i = k;
    while (i > 0) : (it = cdr(it)) {
        i -= 1;
    }
    return car(it);
}

fn list_set(list: Atom, k: usize, value: Atom) void {
    var it = list;
    var i = k;
    while (i > 0) : (it = cdr(it)) {
        i -= 1;
    }
    carP(it).* = value;
}

fn list_reverse(list: *Atom) void {
    var tail = nil;
    var it = list;
    while (!nilp(it.*)) {
        const p = cdr(it.*);
        cdrP(it.*).* = tail;
        tail = it.*;
        it.* = p;
    }
    it.* = tail;
}

fn make_frame(a: Allocator, parent: Atom, env: Atom, tail: Atom) !Atom {
    return cons(a, parent, try cons(a, env, try cons(a, nil, // op
        try cons(a, tail, try cons(a, nil, // args
        try cons(a, nil, // body
        nil))))));
}

fn apply(a: Allocator, f: Atom, args: Atom) !Atom {
    if (f.value == .builtin) {
        var result: Atom = undefined;
        try f.value.builtin(a, args, &result);
        return result;
    } else if (f.value != .closure) return error.Type;

    const env = try env_create(a, car(f));
    var arg_names = car(cdr(f));
    var it = args;
    while (!nilp(arg_names)) : ({
        arg_names = cdr(arg_names);
        it = cdr(it);
    }) {
        if (arg_names.value == .symbol) {
            try env_set(a, env, arg_names, it);
            it = nil;
            break;
        }
        if (nilp(it)) return error.Args;

        try env_set(a, env, car(arg_names), car(it));
    }
    if (!nilp(it)) return error.Args;

    var result: Atom = undefined;

    var body = cdr(cdr(f));
    while (!nilp(body)) : (body = cdr(body)) {
        result = try eval_expr(a, car(body), env);
    }
    return result;
}

fn builtin_car(_: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or !nilp(cdr(args))) return error.Args;

    if (nilp(car(args)))
        result.* = nil
    else if (car(args).value != .pair)
        return error.Type
    else
        result.* = car(car(args));
}

fn builtin_cdr(_: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or !nilp(cdr(args))) return error.Args;

    if (nilp(car(args)))
        result.* = nil
    else if (car(args).value != .pair)
        return error.Type
    else
        result.* = cdr(car(args));
}

fn builtin_cons(a: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    result.* = try cons(a, car(args), car(cdr(args)));
}

fn builtin_add(_: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    if (a.value != .integer or a.value != .integer) return error.Type;

    result.* = make_int(a.value.integer + b.value.integer);
}

fn builtin_subtract(_: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    if (a.value != .integer or a.value != .integer) return error.Type;

    result.* = make_int(a.value.integer - b.value.integer);
}

fn builtin_multiply(_: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    if (a.value != .integer or a.value != .integer) return error.Type;

    result.* = make_int(a.value.integer * b.value.integer);
}

fn builtin_divide(_: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    if (a.value != .integer or a.value != .integer) return error.Type;

    result.* = make_int(@divTrunc(a.value.integer, b.value.integer));
}

fn builtin_numeq(alloc: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    if (a.value != .integer or a.value != .integer) return error.Type;

    result.* = if (a.value.integer == b.value.integer) try make_sym(alloc, "T") else nil;
}

fn builtin_less(alloc: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    if (a.value != .integer or a.value != .integer) return error.Type;

    result.* = if (a.value.integer < b.value.integer) try make_sym(alloc, "T") else nil;
}

fn builtin_apply(a: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const f = car(args);
    const f_args = car(cdr(args));

    if (!listp(f_args)) return error.Syntax;

    result.* = try apply(a, f, f_args);
}

fn builtin_eq(alloc: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    var eq: bool = undefined;
    if (@enumToInt(a.value) == @enumToInt(b.value)) {
        switch (a.value) {
            .nil => eq = true,
            .pair => |pair| eq = pair == b.value.pair,
            .closure => |closure| eq = closure == b.value.closure,
            .macro => |macro| eq = macro == b.value.macro,
            .symbol => |symbol| eq = sliceEql(symbol, b.value.symbol),
            .integer => |integer| eq = integer == b.value.integer,
            .builtin => |builtin| eq = builtin == b.value.builtin,
        }
    } else {
        eq = false;
    }
    result.* = if (eq) try make_sym(alloc, "T") else nil;
}

fn builtin_pairp(a: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or !nilp(cdr(args))) return error.Args;

    result.* = if (car(args).value == .pair) try make_sym(a, "T") else nil;
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

const Error = error{
    Syntax,
    Unbound,
    Args,
    Type,
    OutOfMemory,
};

fn eval_do_exec(stack: *Atom, expr: *Atom, env: *Atom) void {
    env.* = list_get(stack.*, 1);
    var body = list_get(stack.*, 5);
    expr.* = car(body);
    body = cdr(body);
    if (nilp(body)) {
        stack.* = car(stack.*);
    } else {
        list_set(stack.*, 5, body);
    }
}

fn eval_do_bind(a: Allocator, stack: *Atom, expr: *Atom, env: *Atom) !void {
    var body = list_get(stack.*, 5);
    if (!nilp(body)) {
        return eval_do_exec(stack, expr, env);
    }
    const op = list_get(stack.*, 2);
    var args = list_get(stack.*, 4);

    env.* = try env_create(a, car(op));
    var arg_names = car(cdr(op));
    body = cdr(cdr(op));
    list_set(stack.*, 1, env.*);
    list_set(stack.*, 5, body);

    while (!nilp(arg_names)) : ({
        arg_names = cdr(arg_names);
        args = cdr(args);
    }) {
        if (arg_names.value == .symbol) {
            try env_set(a, env.*, arg_names, args);
            args = nil;
            break;
        }

        if (nilp(args)) return error.Args;
        try env_set(a, env.*, car(arg_names), car(args));
    }
    if (!nilp(args)) return error.Args;

    list_set(stack.*, 4, nil);

    return eval_do_exec(stack, expr, env);
}

fn eval_do_apply(a: Allocator, stack: *Atom, expr: *Atom, env: *Atom, result: *Atom) !void {
    _ = result;
    var op = list_get(stack.*, 2);
    var args = list_get(stack.*, 4);

    if (!nilp(args)) {
        list_reverse(&args);
        list_set(stack.*, 4, args);
    }

    if (op.value == .symbol) {
        if (std.mem.eql(u8, op.value.symbol, "APPLY")) {
            stack.* = car(stack.*);
            stack.* = try make_frame(a, stack.*, env.*, nil);
            op = car(args);
            args = car(cdr(args));
            if (!listp(args)) return error.Syntax;

            list_set(stack.*, 2, op);
            list_set(stack.*, 4, args);
        }
    }

    if (op.value == .builtin) {
        stack.* = car(stack.*);
        expr.* = try cons(a, op, args);
        return;
    } else if (op.value != .closure) return error.Type;

    return eval_do_bind(a, stack, expr, env);
}

fn eval_do_return(a: Allocator, stack: *Atom, expr: *Atom, env: *Atom, result: *Atom) !void {
    env.* = list_get(stack.*, 1);
    var op = list_get(stack.*, 2);
    var body = list_get(stack.*, 5);
    var args: Atom = undefined;

    if (!nilp(body)) return eval_do_apply(a, stack, expr, env, result);

    if (nilp(op)) {
        op = result.*;
        list_set(stack.*, 2, op);

        if (op.value == .macro) {
            args = list_get(stack.*, 3);
            stack.* = try make_frame(a, stack.*, env.*, nil);
            const macro = op.value.macro;
            op.value = .{ .closure = macro };
            list_set(stack.*, 2, op);
            list_set(stack.*, 4, args);
            return eval_do_bind(a, stack, expr, env);
        }
    } else if (op.value == .symbol) {
        if (std.mem.eql(u8, op.value.symbol, "DEFINE")) {
            const sym = list_get(stack.*, 4);
            try env_set(a, env.*, sym, result.*);
            stack.* = car(stack.*);
            expr.* = try cons(a, try make_sym(a, "QUOTE"), try cons(a, sym, nil));
            return;
        } else if (std.mem.eql(u8, op.value.symbol, "IF")) {
            args = list_get(stack.*, 3);
            expr.* = if (nilp(result.*)) car(cdr(args)) else car(args);
            stack.* = car(stack.*);
            return;
        } else {
            // goto store_arg;
            args = list_get(stack.*, 4);
            list_set(stack.*, 4, try cons(a, result.*, args));
        }
    } else if (op.value == .macro) {
        expr.* = result.*;
        stack.* = car(stack.*);
        return;
    } else {
        // store_arg:
        args = list_get(stack.*, 4);
        list_set(stack.*, 4, try cons(a, result.*, args));
    }

    args = list_get(stack.*, 3);
    if (nilp(args)) return eval_do_apply(a, stack, expr, env, result);

    expr.* = car(args);
    list_set(stack.*, 3, cdr(args));
}

fn eval_expr(a: Allocator, expr_arg: Atom, env_arg: Atom) Error!Atom {
    var expr = expr_arg;
    var env = env_arg;
    var result: Atom = undefined;
    var stack = nil;
    while (true) {
        if (expr.value == .symbol) {
            result = try env_get(env, expr);
        } else if (expr.value != .pair) {
            result = expr;
        } else if (!listp(expr))
            return error.Syntax
        else {
            const op = car(expr);
            const args = cdr(expr);

            if (op.value == .symbol) {
                // evaluate special forms
                if (std.mem.eql(u8, op.value.symbol, "QUOTE")) {
                    if (nilp(args) or !nilp(cdr(args))) return error.Args;

                    result = car(args);
                } else if (std.mem.eql(u8, op.value.symbol, "DEFINE")) {
                    if (nilp(args) or nilp(cdr(args))) return error.Args;

                    var sym = car(args);
                    if (sym.value == .pair) {
                        result = try make_closure(a, env, cdr(sym), cdr(args));
                        sym = car(sym);
                        if (sym.value != .symbol) return error.Type;
                        try env_set(a, env, sym, result);
                        result = sym;
                    } else if (sym.value == .symbol) {
                        if (!nilp(cdr(cdr(args)))) return error.Args;

                        stack = try make_frame(a, stack, env, nil);
                        list_set(stack, 2, op);
                        list_set(stack, 4, sym);
                        expr = car(cdr(args));
                        continue;
                    } else return error.Type;
                } else if (std.mem.eql(u8, op.value.symbol, "LAMBDA")) {
                    if (nilp(args) or nilp(cdr(args))) return error.Args;
                    result = try make_closure(a, env, car(args), cdr(args));
                } else if (std.mem.eql(u8, op.value.symbol, "IF")) {
                    if (nilp(args) or nilp(cdr(args)) or nilp(cdr(cdr(args))) or !nilp(cdr(cdr(cdr(args)))))
                        return error.Args;

                    stack = try make_frame(a, stack, env, cdr(args));
                    list_set(stack, 2, op);
                    expr = car(args);
                    continue;
                } else if (std.mem.eql(u8, op.value.symbol, "DEFMACRO")) {
                    if (nilp(args) or nilp(cdr(args))) return error.Args;

                    if (car(args).value != .pair) return error.Syntax;

                    const name = car(car(args));
                    if (name.value != .symbol) return error.Type;

                    const macro_closure = try make_closure(a, env, cdr(car(args)), cdr(args));
                    const macro = Atom{ .value = .{ .macro = macro_closure.value.closure } };
                    result = name;
                    try env_set(a, env, name, macro);
                } else if (std.mem.eql(u8, op.value.symbol, "APPLY")) {
                    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;
                    stack = try make_frame(a, stack, env, cdr(args));
                    list_set(stack, 2, op);
                    expr = car(args);
                    continue;
                } else {
                    // goto push;
                    stack = try make_frame(a, stack, env, args);
                    expr = op;
                    continue;
                }
            } else if (op.value == .builtin) {
                try op.value.builtin(a, args, &result);
            } else {
                // push:
                stack = try make_frame(a, stack, env, args);
                expr = op;
                continue;
            }
        }

        if (nilp(stack))
            break;

        try eval_do_return(a, &stack, &expr, &env, &result);
    }
    return result;
}

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const a = arena.allocator();

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

    try env_set(a, env, try make_sym(a, "APPLY"), make_builtin(builtin_apply));
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
            const message = switch (err) {
                error.Syntax => "Syntax error",
                error.OutOfMemory => "Out of memory!",
            };
            try stdout.writeAll(message);
            try stdout.writeByte('\n');
            continue;
        };
        const result = eval_expr(a, expr, env) catch |err| {
            const message = switch (err) {
                error.Syntax => "Syntax error",
                error.Unbound => "Symbol not bound",
                error.Args => "Wrong number of arguments",
                error.Type => "Wrong type",
                error.OutOfMemory => "Out of memory!",
            };
            try stdout.writeAll(message);
            try stdout.writeByte('\n');
            try stdout.writeAll("Evaluating:\n\t");
            try print_expr(expr);
            try stdout.writeByte('\n');
            continue;
        };
        try print_expr(result);
        try stdout.writeByte('\n');
    }
}
