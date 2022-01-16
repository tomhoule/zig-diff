//!
//! ## Naming of arguments and variables
//!
//! - `a` and `b` correspond to the A and B sequences in the Myers paper. Their
//!   respective lengths are M and N.
//! - `k` is a diagonal in the grid. k ranges from -M to N.
//! - `d` corresponds to D in the paper: the length of the shortest edit script
//!   (diff) between A and B. The length of an edit script is the sum of the
//!   length of the ranges it contains. If L is the longest common subsequence
//!   of A and B, then D = 2(N - L).
//!   + When L = 0, then D = 2N, which makes intuitive sense: Delete(A),
//!     Insert(B).
//!   + When L = len(A), then D = 2(N - len(A)). Intuitively, it corresponds to
//!     an Equal edit of length len(A), and between 0 and 2 Insert edits.
//!   Equivalently, D is the length of a D-path.
//!
//! ## Diffs
//! - Always deletes, then inserts.

const std = @import("std");
const testing = std.testing;
const unicode = std.unicode;

const EditType = enum { Delete, Insert, Equal };

const Edit = struct {
    type: EditType,
    /// .{start, end} offsets.
    range: [2]u32,

    pub fn newDelete(start: u32, end: u32) @This() {
        return Edit{ .type = .Delete, .range = .{ start, end } };
    }

    pub fn newInsert(start: u32, end: u32) @This() {
        return Edit{ .type = .Insert, .range = .{ start, end } };
    }

    pub fn newEqual(start: u32, end: u32) @This() {
        return Edit{ .type = .Equal, .range = .{ start, end } };
    }
};

pub fn diff(a: []const u8, b: []const u8, list: *std.ArrayList(Edit)) anyerror!void {
    const aView = try std.unicode.Utf8View.init(a[0..]);
    const bView = try std.unicode.Utf8View.init(b[0..]);
    var aIter = aView.iterator();
    var bIter = bView.iterator();
    const commonPrefix = findCommonPrefix(&aIter, &bIter);

    if (commonPrefix > 0) {
        try list.append(Edit{ .type = .Equal, .range = .{ 0, commonPrefix } });
    }

    if (a.len == commonPrefix) {
        if (b.len != commonPrefix) {
            try list.append(Edit.newInsert(commonPrefix, @intCast(u32, b.len) - commonPrefix));
        }
        return;
    } else if (b.len == commonPrefix) {
        try list.append(Edit.newDelete(commonPrefix, @intCast(u32, a.len) - commonPrefix));
        return;
    }

    try bisect(a, b, list);
}

// Find the middle snake.
fn bisect(a: []const u8, b: []const u8, list: *std.ArrayList(Edit)) !void {
    // TODO: remove.
    const ally = std.heap.page_allocator;

    // Since D = 2(N - L), if the strings have nothing in common (L=0), D = N =
    // len(A) + len(B). We divide by two because we are using a divide and
    // conquer approach, where we find the middle snake starting simultaneously
    // in opposite directions in the graph.
    const max_d = (a.len + b.len + 1) / 2;

    // V arrays.
    //
    // They record the endpoint of all D-paths for a given length. Since every
    // iteration of D uses only odd, or only even indices (lemma 1), they are used
    // to store the results for D paths and (D+1) paths at the same time. See
    // page 6 of the paper for more details.
    //
    // Note that they only store the x component of the furthest reaching
    // D path for each diagonal: the y component can be inferred from the
    // index, since the indices correspond to diagonals.
    //
    // We have two of these: one for the forward path and one for the reverse
    // path.
    const v_offset = max_d;
    // The indices of V should be thought of as ranging [-max_d, max_d].
    const v_len = 2 * max_d;

    // The V array for **forward** paths.
    var v1 = try std.ArrayList(isize).initCapacity(ally, v_len);
    // The V array for **reverse** paths.
    var v2 = try std.ArrayList(isize).initCapacity(ally, v_len);
    defer v1.deinit();
    defer v2.deinit();

    {
        var i: usize = 0;
        while (i < v_len) : (i += 1) {
            v1.appendAssumeCapacity(-1);
            v2.appendAssumeCapacity(-1);
        }
    }

    // We start at (0, 0) and (N, M). At these points (equivalently, for these
    // diagonals), we know there are 0-paths, so we can already fill that in.
    v1[v_offset + 1] = 0;
    v2[v_offset + 1] = 0;

    // The center (0) for the k diagonals in the **reverse** direction. This is
    // used to translate diagonals between the forward and reverse paths.
    // Finding the middle snake means finding a diagonal, so this is important.
    const delta: isize = @intCast(isize, a.len) - @intCast(isize, b.len);

    // If the total number of characters is odd, then the forward path will
    // collide with the reverse path.
    const front = @mod(delta, @as(isize, 2)) != 0;

    // Offsets for start and end of k loop.
    // Prevents mapping of space beyond the grid. k variables are diagonals. k1
    // is the forward path, k2 is the reverse path.
    var k1start: isize = 0;
    var k1end: isize = 0;
    var k2start: isize = 0;
    var k2end: isize = 0;

    // The size of the D-paths we consider.
    var d: isize = 0;

    // Walk D-paths starting from (0, 0) (forward path) and (N, M)
    // (reverse path) until they meet and form a (potentially empty) middle
    // snake.
    while (d < max_d) : (d += 1) {

        // Walk the forward path one step.
        // Every second k (diagonal) is considered, because that is where a D
        // path of length d must end (lemma 1 in the paper).
        var k1 = -d + k1start;
        while (k1 <= d - k1end) : (k1 += 2) {
            const k1_offset = @intCast(usize, (@intCast(isize, v_offset) + k1));
            // ????
            var x1 = if (k1 == -d or (k1 != d and
                v1.items[k1_offset - 1] < v1.items[k1_offset + 1]))
                v1.items[k1_offset + 1]
            else
                v1.items[k1_offset - 1] + 1;

            // Intuition: the diagonal that goes through (0, 0) is k = 0.
            // There, x = y (makes sense visually). The ones above and below it
            // have k = 1 or k = -1, respectively, and there y is one-removed
            // from x. As we move away from the (0, 0) diagonal, the distance
            // grows.
            var y1 = (x1 - k1);

            // // We have the end of the D-path: (x1, y1). Now let's extend it
            // // with its snake.
            // if let (Some(s1), Some(s2)) = (text1.get(x1..), text2.get(y1..)) {
            // let advance = common_prefix_bytes(s1, s2);
            // x1 += advance;
            // y1 += advance;
            // }

            v1.items[k1_offset] = x1;

            if (x1 > a.len) {
                // Ran off the right of the graph. We don't need to consider
                // this diagonal anymore in subsequent iterations.
                k1end += 2;
            } else if (y1 > b.len) {
                // Ran off the bottom of the graph. We don't need to consider
                // this diagonal anymore in subsequent iterations.
                k1start += 2;
            } else if (front) {
                const k2_offset: isize = @intCast(isize, v_offset) + @intCast(isize, delta) - @intCast(isize, k1);
                if (k2_offset >= 0 and k2_offset < v_len and v2.items[@intCast(usize, k2_offset)] != -1) {
                    // Mirror x2 onto top-left coordinate system.
                    const x2 = a.len - @intCast(usize, v2.items[@intCast(usize, k2_offset)]);
                    if (x1 >= x2) {
                        // Overlap detected.
                        // return bisect_split(text1, text2, x1, y1);
                        unreachable;
                    }
                }
            }
        }
    }

    // If we haven't returned earlier, the number of edits equals number of
    // characters, no commonality at all.
    try list.appendSlice(&.{ Edit.newDelete(0, @intCast(u32, a.len)), Edit.newInsert(0, @intCast(u32, b.len)) });
}

fn cloneUtf8Iterator(it: std.unicode.Utf8Iterator) std.unicode.Utf8Iterator {
    return std.unicode.Utf8Iterator{ .bytes = it.bytes, .i = it.i };
}

/// Returns the common suffix length in _bytes_. It also advances the iterators.
fn findCommonSuffixBytes(a: []const u8, b: []const u8) u32 {
    const max = std.math.min(a.len, b.len);
    var i = 0;
    while (i <= max and a[a.len - (i + 1)] == b[b.len - (i + 1)]) : (i += 1) {}
    return i;
}

/// Returns the common suffix length in _bytes_. It also advances the iterators.
fn findCommonPrefixBytes(a: []const u8, b: []const u8) u32 {
    const max = std.math.min(a.len, b.len);
    var i = 0;
    while (i <= max and a[i] == b[i]) : (i += 1) {}
    return i;
}

/// Returns the common prefix length in _bytes_. It also advances the iterators.
fn findCommonPrefix(a: *std.unicode.Utf8Iterator, b: *std.unicode.Utf8Iterator) u32 {
    // First, determine the common prefix as an optimization.
    var commonPrefix: u32 = 0;

    while (a.nextCodepointSlice()) |aNext| {
        const bNext = b.nextCodepointSlice() orelse break;

        if (!std.mem.eql(u8, aNext, bNext)) {
            break;
        }

        commonPrefix += @intCast(u32, aNext.len);
    }

    return commonPrefix;
}

// ported from dtolnay/dissimilar: https://github.com/dtolnay/dissimilar/blob/master/tests/test.rs
test "diff emojis" {
    const ally = testing.allocator;
    // Unicode snowman and unicode comet have the same first two bytes. A
    // byte-based diff would produce a 2-byte Equal followed by 1-byte Delete
    // and Insert.
    var snowman = "\u{2603}".*;
    var comet = "\u{2604}".*;
    try testing.expectEqualSlices(u8, snowman[0..2], comet[0..2]);

    var diffBuf = std.ArrayList(Edit).init(ally);
    defer diffBuf.deinit();
    try diff(snowman[0..], comet[0..], &diffBuf);

    const expected: []const Edit = &.{ Edit{ .type = .Delete, .range = .{ 0, 3 } }, Edit{ .type = .Insert, .range = .{ 0, 3 } } };

    try testing.expectEqualSlices(Edit, expected, diffBuf.items);
}

// ported from dtolnay/dissimilar: https://github.com/dtolnay/dissimilar/blob/master/tests/test.rs
test "diff emojis with longer string" {
    try expectDiffRoundtrip("$=[$-乀丁$+一$=abcd$-一$+丁$=]");
}

test "compileDiffSpec works" {
    const ds1 = compileDiffSpec("$=abcd");
    const ds1_exp: []const Edit = &.{Edit{ .type = .Equal, .range = .{ 0, 4 } }};
    try testing.expectEqualSlices(Edit, ds1_exp, ds1.diff);

    const ds2 = compileDiffSpec("$-abcd$+efgh");
    const ds2_exp: []const Edit = &.{ Edit{ .type = .Delete, .range = .{ 0, 4 } }, Edit{ .type = .Insert, .range = .{ 0, 4 } } };
    try testing.expectEqualSlices(Edit, ds2_exp, ds2.diff);

    const ds3 = compileDiffSpec("$-abcd$+efgh$= $-aiue$+ζιγ");
    const ds3_exp: []const Edit = &.{ Edit{ .type = .Delete, .range = .{ 0, 4 } }, Edit{ .type = .Insert, .range = .{ 0, 4 } }, Edit{ .type = .Equal, .range = .{ 4, 5 } }, Edit{ .type = .Delete, .range = .{ 5, 9 } }, Edit{ .type = .Insert, .range = .{ 5, 11 } } };
    try testing.expectEqualSlices(Edit, ds3_exp, ds3.diff);
}

test "basic diff tests" {
    try expectDiffRoundtrip("$-meow$+woofwoof");
}

fn expectDiffRoundtrip(comptime spec: []const u8) anyerror!void {
    const ds = comptime compileDiffSpec(spec);
    const ally = testing.allocator;
    var diffed = std.ArrayList(Edit).init(ally);
    defer diffed.deinit();
    try diff(ds.a, ds.b, &diffed);
    try testing.expectEqualSlices(Edit, ds.diff, diffed.items) catch |err| blk: {
        var dsDiff = std.ArrayList(u8).init(ally);
        defer dsDiff.deinit();
        try debugFmtDiff(ds.a, ds.b, ds.diff, dsDiff.writer());
        var diffedDiff = std.ArrayList(u8).init(ally);
        try debugFmtDiff(ds.a, ds.b, diffed.items, diffedDiff.writer());
        defer diffedDiff.deinit();
        std.log.err(
            \\
            \\Input a:
            \\
            \\{s}
            \\
            \\Input b:
            \\
            \\{s}
            \\
            \\Expected diff:
            \\
            \\{s}
            \\
            \\Found diff:
            \\
            \\{s}
        , .{ ds.a, ds.b, dsDiff.items, diffedDiff.items });
        break :blk err;
    };
}

fn debugFmtDiff(a: []const u8, b: []const u8, edits: []const Edit, out: anytype) !void {
    for (edits) |edit| {
        switch (edit.type) {
            .Equal => try out.writeAll(a[edit.range[0]..edit.range[1]]),
            .Delete => try out.print("\x1b[41m{s}\x1b[0m", .{a[edit.range[0]..edit.range[1]]}),
            .Insert => try out.print("\x1b[42m{s}\x1b[0m", .{b[edit.range[0]..edit.range[1]]}),
        }
    }
}

const DiffSpec = struct {
    diff: []const Edit,
    a: []const u8,
    b: []const u8,

    fn debugFmt(comptime this: @This()) []const u8 {
        comptime {
            var out: []const u8 = "";

            for (this.diff) |edit| {
                const chunk = switch (edit.type) {
                    .Equal => this.a[edit.range[0]..edit.range[1]],
                    .Delete => std.fmt.comptimePrint("\x1b[41m{s}\x1b[0m", .{this.a[edit.range[0]..edit.range[1]]}),
                    .Insert => std.fmt.comptimePrint("\x1b[42m{s}\x1b[0m", .{this.b[edit.range[0]..edit.range[1]]}),
                };

                out = out ++ chunk;
            }

            return out;
        }
    }
};

fn compileDiffSpec(comptime expected: []const u8) DiffSpec {
    comptime {
        const utf8ExpectedView = unicode.Utf8View.init(expected) catch {
            @compileError("not utf8: " ++ expected);
        };
        var utf8expected = utf8ExpectedView.iterator();
        const firstChar = utf8expected.nextCodepointSlice() orelse @compileError("empty diffspec");
        var editType = blk: {
            if (std.mem.eql(u8, firstChar, "$")) {
                const nextChar = utf8expected.nextCodepointSlice() orelse @compileError("srsly");
                break :blk charToEdit(nextChar);
            } else {
                @compileError("diff spec must start with '$' character");
            }
        };
        var aRangeStart = 0;
        var bRangeStart = 0;
        var edits: []const Edit = &.{};
        var a: []const u8 = &.{};
        var b: []const u8 = &.{};

        while (utf8expected.nextCodepointSlice()) |c| {
            switch (c[0]) {
                '$' => {
                    // end of current edit
                    const nextEdit = makeEdit(editType, aRangeStart, a.len, bRangeStart, b.len);
                    edits = edits ++ [_]Edit{nextEdit};
                    aRangeStart = a.len;
                    bRangeStart = b.len;

                    // beginning of the next one
                    const editChar = utf8expected.nextCodepointSlice() orelse @compileError("$ at end of dffspec");
                    editType = charToEdit(editChar);
                },
                else => {
                    switch (editType) {
                        .Equal => {
                            a = a ++ c;
                            b = b ++ c;
                        },
                        .Delete => {
                            a = a ++ c;
                        },
                        .Insert => {
                            b = b ++ c;
                        },
                    }
                },
            }
        }

        const lastEdit = makeEdit(editType, aRangeStart, a.len, bRangeStart, b.len);
        edits = edits ++ [_]Edit{lastEdit};

        const spec = DiffSpec{ .diff = edits, .a = a, .b = b };
        return spec;
    }
}

fn makeEdit(editType: EditType, aRangeStart: u32, aRangeEnd: u32, bRangeStart: u32, bRangeEnd: u32) Edit {
    const range = switch (editType) {
        .Equal => [2]u32{ aRangeStart, aRangeEnd },
        .Insert => [2]u32{ bRangeStart, bRangeEnd },
        .Delete => [2]u32{ aRangeStart, aRangeEnd },
    };
    return Edit{ .type = editType, .range = range };
}

fn charToEdit(comptime c: []const u8) EditType {
    if (c.len > 1) {
        @compileError("Invalid char after a $");
    }

    return switch (c[0]) {
        '+' => .Insert,
        '=' => .Equal,
        '-' => .Delete,
        else => @compileError("Unknown diff specifier"),
    };
}
