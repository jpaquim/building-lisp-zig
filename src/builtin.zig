const std = @import("std");
const Allocator = std.mem.Allocator;

const data = @import("./data.zig");
const cons = data.cons;
const make_int = data.make_int;
const make_sym = data.make_sym;

const lisp = @import("./lisp.zig");
const Atom = lisp.Atom;
const car = lisp.car;
const cdr = lisp.cdr;
const nil = lisp.nil;
const nilp = lisp.nilp;

const sliceEql = @import("./util.zig").sliceEql;

pub fn builtin_car(_: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or !nilp(cdr(args))) return error.Args;

    if (nilp(car(args)))
        result.* = nil
    else if (car(args).value != .pair)
        return error.Type
    else
        result.* = car(car(args));
}

pub fn builtin_cdr(_: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or !nilp(cdr(args))) return error.Args;

    if (nilp(car(args)))
        result.* = nil
    else if (car(args).value != .pair)
        return error.Type
    else
        result.* = cdr(car(args));
}

pub fn builtin_cons(a: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    result.* = try cons(a, car(args), car(cdr(args)));
}

pub fn builtin_add(_: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    if (a.value != .integer or a.value != .integer) return error.Type;

    result.* = make_int(a.value.integer + b.value.integer);
}

pub fn builtin_subtract(_: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    if (a.value != .integer or a.value != .integer) return error.Type;

    result.* = make_int(a.value.integer - b.value.integer);
}

pub fn builtin_multiply(_: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    if (a.value != .integer or a.value != .integer) return error.Type;

    result.* = make_int(a.value.integer * b.value.integer);
}

pub fn builtin_divide(_: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    if (a.value != .integer or a.value != .integer) return error.Type;

    result.* = make_int(@divTrunc(a.value.integer, b.value.integer));
}

pub fn builtin_numeq(alloc: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    if (a.value != .integer or a.value != .integer) return error.Type;

    result.* = if (a.value.integer == b.value.integer) try make_sym(alloc, "T") else nil;
}

pub fn builtin_less(alloc: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or nilp(cdr(args)) or !nilp(cdr(cdr(args)))) return error.Args;

    const a = car(args);
    const b = car(cdr(args));

    if (a.value != .integer or a.value != .integer) return error.Type;

    result.* = if (a.value.integer < b.value.integer) try make_sym(alloc, "T") else nil;
}

pub fn builtin_eq(alloc: Allocator, args: Atom, result: *Atom) !void {
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

pub fn builtin_pairp(a: Allocator, args: Atom, result: *Atom) !void {
    if (nilp(args) or !nilp(cdr(args))) return error.Args;

    result.* = if (car(args).value == .pair) try make_sym(a, "T") else nil;
}
