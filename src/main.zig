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

const Error = error{
    Syntax,
    OutOfMemory,
};

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
        .symbol => {
            try stdout.writeAll(atom.value.symbol);
        },
        .integer => {
            try stdout.print("{}", .{atom.value.integer});
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
    const prefix = "()";

    const start = strspn(str, ws);

    if (start == str.len) return error.Syntax;

    const end = if (strchr(prefix, str[start]) != null)
        start + 1
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

fn read_list(a: Allocator, input: []const u8, end_ptr: *[]const u8) Error!Atom {
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

fn read_expr(a: Allocator, input: []const u8, end_ptr: *[]const u8) Error!Atom {
    const token = try lex(input, end_ptr);
    if (token[0] == '(') {
        const str = end_ptr.*;
        return read_list(a, str, end_ptr);
    } else if (token[0] == ')') {
        return error.Syntax;
    } else {
        return parse_simple(a, token);
    }
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

    while (true) {
        try stdout.writeAll("> ");

        const input = (try stdin.readUntilDelimiterOrEof(&input_buffer, '\n')) orelse {
            // reached input end-of-file
            break;
        };
        var end = input;
        const expr = read_expr(a, input, &end) catch |err| {
            switch (err) {
                error.Syntax => {
                    try stdout.writeAll("Syntax error\n");
                    break;
                },
                error.OutOfMemory => {
                    try stdout.writeAll("Out of memory!\n");
                    break;
                },
            }
        };
        try print_expr(expr);
        try stdout.writeByte('\n');
    }
}
