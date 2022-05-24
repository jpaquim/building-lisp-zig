const std = @import("std");
const Allocator = std.mem.Allocator;

const lisp = @import("./lisp.zig");
const Atom = lisp.Atom;
const Builtin = lisp.Builtin;
const Pair = lisp.Pair;
const car = lisp.car;
const carP = lisp.carP;
const cdr = lisp.cdr;
const cdrP = lisp.cdrP;
const nil = lisp.nil;
const nilp = lisp.nilp;

const Allocation = struct {
    pair: Pair,
    mark: bool,
    next: ?*Allocation,
};

var global_allocations: ?*Allocation = null;

pub fn cons(a: Allocator, car_val: Atom, cdr_val: Atom) !Atom {
    var allocation = try a.create(Allocation);
    allocation.mark = false;
    allocation.next = global_allocations;
    global_allocations = allocation;

    allocation.pair = .{ .atom = .{ car_val, cdr_val } };
    return Atom{ .value = .{ .pair = &allocation.pair } };
}

pub fn make_int(x: i64) Atom {
    return .{ .value = .{ .integer = x } };
}

var sym_table = nil;

pub fn make_sym(a: Allocator, s: []const u8) !Atom {
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

pub fn make_builtin(f: Builtin) Atom {
    return .{ .value = .{ .builtin = f } };
}

pub fn listp(expr: Atom) bool {
    var it = expr;
    while (!nilp(it)) : (it = cdr(it)) {
        if (it.value != .pair) return false;
    } else return true;
}

pub fn copy_list(a: Allocator, list: Atom) !Atom {
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

pub fn list_get(list: Atom, k: usize) Atom {
    var it = list;
    var i = k;
    while (i > 0) : (it = cdr(it)) {
        i -= 1;
    }
    return car(it);
}

pub fn list_set(list: Atom, k: usize, value: Atom) void {
    var it = list;
    var i = k;
    while (i > 0) : (it = cdr(it)) {
        i -= 1;
    }
    carP(it).* = value;
}

pub fn list_reverse(list: *Atom) void {
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

pub fn gc_mark(root: Atom) void {
    const pair = switch (root.value) {
        .pair => |pair| pair,
        .closure => |closure| closure,
        .macro => |macro| macro,
        else => return,
    };

    const a = @fieldParentPtr(Allocation, "pair", pair);

    if (a.mark) return;

    a.mark = true;

    gc_mark(car(root));
    gc_mark(cdr(root));
}

pub fn gc(alloc: Allocator) void {
    gc_mark(sym_table);

    var p = &global_allocations;
    while (p.*) |a| {
        if (!a.mark) {
            p.* = a.next;
            alloc.destroy(a);
        } else {
            p = &a.next;
        }
    }
    var a = global_allocations;
    while (a) |allocation| : (a = allocation.next) {
        allocation.mark = false;
    }
}

pub fn deinit(alloc: Allocator) void {
    var it = sym_table;
    while (!nilp(it)) : (it = cdr(it)) {
        const sym = car(it);
        alloc.free(sym.value.symbol);
    }

    var p = &global_allocations;
    while (p.*) |a| {
        p.* = a.next;
        alloc.destroy(a);
    }
}
