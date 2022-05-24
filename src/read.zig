const std = @import("std");
const Allocator = std.mem.Allocator;

const data = @import("./data.zig");
const cons = data.cons;
const make_int = data.make_int;
const make_sym = data.make_sym;

const lisp = @import("./lisp.zig");
const Atom = lisp.Atom;
const car = lisp.car;
const carP = lisp.carP;
const cdr = lisp.cdr;
const cdrP = lisp.cdrP;
const nil = lisp.nil;
const nilp = lisp.nilp;

const util = @import("./util.zig");
const strchr = util.strchr;
const strcspn = util.strcspn;
const strspn = util.strspn;

const ReadError = error{
    Syntax,
    OutOfMemory,
};

pub fn read_expr(a: Allocator, input: []const u8, end_ptr: *[]const u8) ReadError!Atom {
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
