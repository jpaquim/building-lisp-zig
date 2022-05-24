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

pub const Atom = struct {
    value: Value,
};

pub const Pair = struct {
    atom: [2]Atom,
};

pub const Builtin = fn (a: Allocator, args: Atom, result: *Atom) Error!void;

pub const Error = error{
    Syntax,
    Unbound,
    Args,
    Type,
    OutOfMemory,
};

pub fn car(p: Atom) Atom {
    const pair = if (p.value == .pair) p.value.pair else if (p.value == .closure) p.value.closure else p.value.macro;
    return pair.atom[0];
}

pub fn carP(p: Atom) *Atom {
    const pair = if (p.value == .pair) p.value.pair else if (p.value == .closure) p.value.closure else p.value.macro;
    return &pair.atom[0];
}

pub fn cdr(p: Atom) Atom {
    const pair = if (p.value == .pair) p.value.pair else if (p.value == .closure) p.value.closure else p.value.macro;
    return pair.atom[1];
}

pub fn cdrP(p: Atom) *Atom {
    const pair = if (p.value == .pair) p.value.pair else if (p.value == .closure) p.value.closure else p.value.macro;
    return &pair.atom[1];
}

pub fn nilp(atom: Atom) bool {
    return atom.value == .nil;
}

pub const nil = Atom{ .value = .nil };
