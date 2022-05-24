pub fn strspn(s: []const u8, accept: []const u8) usize {
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        for (accept) |char| {
            if (s[i] == char) break;
        } else return i;
    }
    return i;
}

pub fn strcspn(s: []const u8, reject: []const u8) usize {
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        for (reject) |char| {
            if (s[i] == char) return i;
        }
    }
    return i;
}

pub fn strchr(s: []const u8, c: u8) ?*const u8 {
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        if (s[i] == c) return &s[i];
    } else return null;
}

pub fn sliceEql(a: []const u8, b: []const u8) bool {
    return a.ptr == b.ptr and a.len == b.len;
}
