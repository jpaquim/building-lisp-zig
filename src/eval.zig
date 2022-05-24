const std = @import("std");
const Allocator = std.mem.Allocator;

const data = @import("./data.zig");
const cons = data.cons;
const gc = data.gc;
const gc_mark = data.gc_mark;
const list_get = data.list_get;
const list_reverse = data.list_reverse;
const list_set = data.list_set;
const listp = data.listp;
const make_sym = data.make_sym;

const lisp = @import("./lisp.zig");
const Atom = lisp.Atom;
const Error = lisp.Error;
const car = lisp.car;
const cdr = lisp.cdr;
const cdrP = lisp.cdrP;
const nil = lisp.nil;
const nilp = lisp.nilp;

const sliceEql = @import("./util.zig").sliceEql;

const count_gc = 100000;

var count: usize = 0;

pub fn eval_expr(a: Allocator, expr_arg: Atom, env_arg: Atom) Error!Atom {
    var expr = expr_arg;
    var env = env_arg;
    var result: Atom = undefined;
    var stack = nil;
    while (true) {
        count += 1;
        if (count == count_gc) {
            gc_mark(expr);
            gc_mark(env);
            gc_mark(stack);
            gc(a);
            count = 0;
        }
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

pub fn env_create(a: Allocator, parent: Atom) !Atom {
    return cons(a, parent, nil);
}

pub fn env_get(env: Atom, symbol: Atom) error{Unbound}!Atom {
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

pub fn env_set(a: Allocator, env: Atom, symbol: Atom, value: Atom) !void {
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

fn make_frame(a: Allocator, parent: Atom, env: Atom, tail: Atom) !Atom {
    return cons(a, parent, try cons(a, env, try cons(a, nil, // op
        try cons(a, tail, try cons(a, nil, // args
        try cons(a, nil, // body
        nil))))));
}

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
