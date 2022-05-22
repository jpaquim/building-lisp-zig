const std = @import("std");
const Allocator = std.mem.Allocator;

const Atom = struct {
    value: union(enum) {
        nil,
        pair: *Pair,
        symbol: []const u8,
        integer: i64,
    },
};

const Pair = struct {
    atom: [2]Atom,
};

fn car(p: Atom) Atom {
    return p.value.pair.atom[0];
}

fn carP(p: Atom) *Atom {
    return &p.value.pair.atom[0];
}

fn cdr(p: Atom) Atom {
    return p.value.pair.atom[1];
}

fn cdrP(p: Atom) *Atom {
    return &p.value.pair.atom[1];
}

fn nilp(atom: Atom) bool {
    return atom.value == .nil;
}

const nil = Atom{ .value = .nil };

fn cons(a: Allocator, car_val: Atom, cdr_val: Atom) !Atom {
    var pair: Atom = undefined;
    pair.value = .{ .pair = try a.create(Pair) };
    carP(pair).* = car_val;
    cdrP(pair).* = cdr_val;
    return pair;
}

fn make_int(x: i64) Atom {
    var atom: Atom = undefined;
    atom.value = .{ .integer = x };
    return atom;
}

fn make_sym(a: Allocator, s: []const u8) !Atom {
    var atom: Atom = undefined;
    atom.value = .{ .symbol = try a.dupe(u8, s) };
    return atom;
}

const Error = std.fs.File.WriteError;

fn print_expr(atom: Atom) Error!void {
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
        .symbol => {
            try stdout.writeAll(atom.value.symbol);
        },
        .integer => {
            try stdout.print("{}", .{atom.value.integer});
        },
    }
}

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const a = arena.allocator();

    const stdout = std.io.getStdOut().writer();

    try print_expr(make_int(42));
    try stdout.writeByte('\n');

    try print_expr(try make_sym(a, "FOO"));
    try stdout.writeByte('\n');

    try print_expr(try cons(a, try make_sym(a, "X"), try make_sym(a, "Y")));
    try stdout.writeByte('\n');

    try print_expr(try cons(a, make_int(1), try cons(a, make_int(2), try cons(a, make_int(3), nil))));
    try stdout.writeByte('\n');
}
