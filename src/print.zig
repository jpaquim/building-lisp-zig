const std = @import("std");

const lisp = @import("./lisp.zig");
const Atom = lisp.Atom;
const car = lisp.car;
const cdr = lisp.cdr;
const nilp = lisp.nilp;

const PrintError = std.fs.File.WriteError;

pub fn print_expr(atom: Atom) PrintError!void {
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
