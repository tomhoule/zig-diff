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

const std = @import("std");
const debug = std.debug;
const testing = std.testing;
const unicode = std.unicode;
const ends = @import("./ends.zig");
const Range = @import("range.zig").Range;

const EditType = enum { Delete, Insert, Equal };

const Edit = struct {
    type: EditType,
    /// .{start, end} offsets.
    range: [2]u32,

    const Self = @This();

    pub fn newDelete(range: Range) Self {
        return Edit{ .type = .Delete, .range = .{ @intCast(u32, range.start), @intCast(u32, range.end) } };
    }

    pub fn newInsert(range: Range) Self {
        return Edit{ .type = .Insert, .range = .{ @intCast(u32, range.start), @intCast(u32, range.end) } };
    }

    pub fn newEqual(range: Range) Self {
        debug.print("Equal range: {s}\n", .{range.subslice()});
        // if (std.mem.eql(u8, range.subslice(), "6xxx")) {
        //     debug.panic("meow", .{});
        // }
        return Edit{ .type = .Equal, .range = .{ @intCast(u32, range.start), @intCast(u32, range.end) } };
    }
};

fn main(a: Range, b: Range, list: *std.ArrayList(Edit)) !void {
    // Handle empty input on either side.
    if (a.len() == 0 and b.len() == 0) {
        return;
    } else if (a.len() == 0) {
        try list.append(Edit.newInsert(b));
        return;
    } else if (b.len() == 0) {
        try list.append(Edit.newDelete(a));
        return;
    }

    const prefix = ends.findCommonPrefixBytes(a, b);
    const suffix = ends.findCommonSuffixBytes(a, b);
    debug.print("Prefix: {s} — Suffix: {s}\n", .{ prefix.subslice(), suffix.subslice() });

    if (prefix.len() > 0) {
        try list.append(Edit.newEqual(prefix));
    }

    if (a.len() == prefix.len()) {
        if (b.len() != prefix.len()) {
            try list.append(Edit.newInsert(b.clampLeft(prefix.len())));
        }
        return;
    } else if (b.len() == prefix.len()) {
        try list.append(Edit.newDelete(a.clampLeft(prefix.len())));
        return;
    }

    if (a.len() == suffix.len()) {
        if (b.len() != suffix.len()) {
            try list.append(Edit.newInsert(b.clampRight(suffix.len())));
        }
        try list.append(Edit.newEqual(a));
        return;
    } else if (b.len() == suffix.len()) {
        try list.append(Edit.newDelete(a.clampRight(suffix.len())));
        try list.append(Edit.newEqual(suffix));
        return;
    }

    const aClamped = a.clamp(prefix.len(), suffix.len());
    const bClamped = b.clamp(prefix.len(), suffix.len());

    if (aClamped.len() == 1 or bClamped.len() == 1) {
        try list.appendSlice(&.{ Edit.newDelete(aClamped), Edit.newInsert(bClamped) });
    } else {
        try bisect(aClamped, bClamped, list);
    }

    if (suffix.len() > 0) {
        try list.append(Edit.newEqual(suffix));
    }
}

pub fn diff(a: []const u8, b: []const u8, list: *std.ArrayList(Edit)) !void {
    const aRange = Range.new(a);
    const bRange = Range.new(b);
    try main(aRange, bRange, list);
}

// Find the middle snake.
fn bisect(a: Range, b: Range, list: *std.ArrayList(Edit)) !void {
    // TODO: remove.
    const ally = std.heap.page_allocator;

    // Since D = 2(N - L), if the strings have nothing in common (L=0), D = N =
    // len(A) + len(B). We divide by two because we are using a divide and
    // conquer approach, where we find the middle snake starting simultaneously
    // in opposite directions in the graph.
    const max_d = (a.len() + b.len() + 1) / 2;

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
        debug.assert(v1.items.len == v_len);
    }

    debug.print("\na.len: {d}, b.len: {d}, v_len: {d}, v_offset: {d}\n", .{ a.len(), a.len(), v_len, v_offset });

    // We start at (0, 0) and (N, M). At these points (equivalently, for these
    // diagonals), we know there are 0-paths, so we can already fill that in.
    v1.items[v_offset + 1] = 0;
    v2.items[v_offset + 1] = 0;

    // The center (0) for the k diagonals in the **reverse** direction. This is
    // used to translate diagonals between the forward and reverse paths.
    // Finding the middle snake means finding a diagonal, so this is important.
    const delta: isize = @intCast(isize, a.len()) - @intCast(isize, b.len());

    // If the total number of characters is odd, then the forward path will
    // collide with the reverse path.
    const front = @mod(delta, 2) != 0;

    // Offsets for start and end of k loop. Prevents mapping of space beyond
    // the grid. k variables are diagonals. k1 is the forward path, k2 is the
    // reverse path.
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
        while (k1 <= d - k1end) {
            const k1_offset = @intCast(usize, (@intCast(isize, v_offset) + k1));
            // We are lengthening a D-path by one: we know we will take a
            // horizontal or a vertical step (if there was a diagonal, it was
            // consumed in the previous iteration and we're now at the end of
            // it).
            //
            // k1 == -d means we're on the leftmost diagonal, we continue down.
            // k1 == d means we're on the rightmost diagonal, we continue right.
            //
            // To understand the V[K - 1] < V[K + 1] condition, lemma 2 is the
            // relevant reading. We take the horizontal or vertical step
            // depending on the shortest path between the one directly on top
            // and the one directly on the left.
            var x1: usize = if (k1 == -d or (k1 != d and
                v1.items[k1_offset - 1] < v1.items[k1_offset + 1]))
                @intCast(usize, v1.items[k1_offset + 1]) // vertical step
            else
                @intCast(usize, v1.items[k1_offset - 1] + 1); // horizontal step

            // Intuition: the diagonal that goes through (0, 0) is k = 0.
            // There, x = y (makes sense visually). The ones above and below it
            // have k = 1 or k = -1, respectively, and there y is one-removed
            // from x. As we move away from the (0, 0) diagonal, the distance
            // grows.
            var y1 = @intCast(usize, @intCast(isize, x1) - k1);

            // We have the end of the D-path: (x1, y1). Now let's extend it
            // with its snake.
            if (x1 < a.len() and y1 < b.len()) {
                const prefix = ends.findCommonPrefixBytes(a.clampLeft(x1), b.clampLeft(y1));
                x1 += prefix.len();
                y1 += prefix.len();
            }

            // We have the new x for the k1 diagonal.
            v1.items[k1_offset] = @intCast(isize, x1);

            if (x1 > a.len()) {
                // Ran off the right of the graph. We don't need to consider
                // this diagonal anymore in subsequent iterations.
                k1end += 2;
            } else if (y1 > b.len()) {
                // Ran off the bottom of the graph. We don't need to consider
                // this diagonal anymore in subsequent iterations.
                k1start += 2;
            } else if (front) {
                // The k1 diagonal on the reverse side.
                const k2_offset: isize = @intCast(isize, v_offset) + @intCast(isize, delta) - @intCast(isize, k1);

                // Do we have a reverse D-path on the k1=k2 diagonal?
                if (k2_offset >= 0 and k2_offset < v_len and v2.items[@intCast(usize, k2_offset)] != -1) {
                    // Mirror x2 onto top-left coordinate system.
                    const x2 = a.len() - @intCast(usize, v2.items[@intCast(usize, k2_offset)]);
                    // Does the reverse path on the same diagonal go all the
                    // way to x1? The forward and reverse paths meet, and we
                    // have found the middle snake!
                    if (x1 >= x2) {
                        // Overlap detected.
                        return bisect_split(a, b, x1, y1, list);
                    }
                }
            }

            k1 += 2;
        }

        // Walk the reverse path one step. This is symmetric to the previous
        // loop, so comments are omitted.
        var k2 = -d + k2start;
        while (k2 <= d - k2end) {
            const k2_offset = @intCast(usize, @intCast(isize, v_offset) + k2);
            var x2: usize = if (k2 == -d or (k2 != d and v2.items[k2_offset - 1] < v2.items[k2_offset + 1])) @intCast(usize, v2.items[k2_offset + 1]) else @intCast(usize, v2.items[k2_offset - 1] + 1);

            var y2: usize = @intCast(usize, @intCast(isize, x2) - k2);

            if (x2 < a.len() and y2 < b.len()) {
                const suffix = ends.findCommonSuffixBytes(a.clampRight(x2), b.clampRight(y2));
                x2 += suffix.len();
                y2 += suffix.len();
            }

            v2.items[k2_offset] = @intCast(isize, x2);

            if (x2 > a.len()) {
                k2end += 2;
            } else if (y2 > b.len()) {
                // Ran off the top of the graph.
                k2start += 2;
            } else if (!front) {
                const k1_offset = @intCast(isize, v_offset) + delta - k2;
                if (k1_offset >= 0 and k1_offset < @intCast(isize, v_len) and v1.items[@intCast(usize, k1_offset)] != -1) {
                    const x1 = @intCast(usize, v1.items[@intCast(usize, k1_offset)]);
                    const y1 = v_offset + x1 - @intCast(usize, k1_offset);
                    // Mirror x2 onto top-left coordinate system.
                    x2 = a.len() - x2;
                    if (x1 >= x2) {
                        return bisect_split(a, b, x1, y1, list);
                    }
                }
            }

            k2 += 2;
        }
    }

    // If we haven't returned earlier, the number of edits equals number of
    // characters, no commonality at all.
    debug.print("hit! {s}\n", .{a.subslice()});
    try list.appendSlice(&.{ Edit.newDelete(a), Edit.newInsert(b) });
}

fn bisect_split(a: Range, b: Range, x1: usize, y1: usize, list: *std.ArrayList(Edit)) anyerror!void {
    var as = a.splitAt(x1);
    var bs = b.splitAt(y1);

    try main(as[0], bs[0], list);
    try main(as[1], bs[1], list);
}

fn cloneUtf8Iterator(it: std.unicode.Utf8Iterator) std.unicode.Utf8Iterator {
    return std.unicode.Utf8Iterator{ .bytes = it.bytes, .i = it.i };
}

// // ported from dtolnay/dissimilar: https://github.com/dtolnay/dissimilar/blob/master/tests/test.rs
// test "diff emojis" {
//     const ally = testing.allocator;
//     // Unicode snowman and unicode comet have the same first two bytes. A
//     // byte-based diff would produce a 2-byte Equal followed by 1-byte Delete
//     // and Insert.
//     var snowman = "\u{2603}";
//     var comet = "\u{2604}";
//     try testing.expectEqualSlices(u8, snowman[0..2], comet[0..2]);

//     var diffBuf = std.ArrayList(Edit).init(ally);
//     defer diffBuf.deinit();
//     try diff(snowman[0..], comet[0..], &diffBuf);

//     const expected: []const Edit = &.{ Edit.newDelete(Range.new(snowman[0..])), Edit.newInsert(Range.new(comet[0..])) };

//     try testing.expectEqualSlices(Edit, expected, diffBuf.items);
// }

// // ported from dtolnay/dissimilar: https://github.com/dtolnay/dissimilar/blob/master/tests/test.rs
// test "diff emojis with longer string" {
//     try expectDiffRoundtrip("$=[$-乀丁$+一$=abcd$-一$+丁$=]");
// }

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

test "diff with common prefix" {
    // no common prefix
    try expectDiffRoundtrip("$-abc$+xyz");

    // common prefix with different suffixes
    try expectDiffRoundtrip("$=1234$-abcdef$+xyz");

    // common prefix with suffix only on the a side
    try expectDiffRoundtrip("$=1234$+xyz");
}

test "diff with common suffix" {
    // no common suffix
    try expectDiffRoundtrip("$-abc$+xyz");

    // common suffix with different prefixes
    try expectDiffRoundtrip("$-abcdef$+xyz$=1234");

    // common suffix with prefix only on the b side
    try expectDiffRoundtrip("$+xyz$=1234");
}

test "basic diff tests" {
    // try expectDiffRoundtrip("$-meow$+woofwoof");
    try expectDiffRoundtrip("$=nononono");
    try expectDiffRoundtrip("$=[w$-a$+u$=t]");
    try expectDiffRoundtrip("$-123456$+abcd");
    try expectDiffRoundtrip("$-123456$=xxx$+abcd");
    try expectDiffRoundtrip("$-f$+\u{fb01}$=i");
}

fn expectDiffRoundtrip(comptime spec: []const u8) !void {
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
            \\ Expected: {s} 
            \\ != 
            \\ Actual: {s}
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
        , .{ ds.diff, diffed.items, ds.a, ds.b, dsDiff.items, diffedDiff.items });
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
