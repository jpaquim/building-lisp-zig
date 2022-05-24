const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Atom = union(enum) {
    nil,
    pair: *Pair,
    symbol: []const u8,
    integer: i64,
    builtin: Builtin,
    closure: *Pair,
    macro: *Pair,
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
    const pair = if (p == .pair) p.pair else if (p == .closure) p.closure else p.macro;
    return pair.atom[0];
}

pub fn carP(p: Atom) *Atom {
    const pair = if (p == .pair) p.pair else if (p == .closure) p.closure else p.macro;
    return &pair.atom[0];
}

pub fn cdr(p: Atom) Atom {
    const pair = if (p == .pair) p.pair else if (p == .closure) p.closure else p.macro;
    return pair.atom[1];
}

pub fn cdrP(p: Atom) *Atom {
    const pair = if (p == .pair) p.pair else if (p == .closure) p.closure else p.macro;
    return &pair.atom[1];
}

pub fn nilp(atom: Atom) bool {
    return atom == .nil;
}

pub const nil: Atom = .nil;
