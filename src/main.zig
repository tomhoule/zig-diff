//! Always deletes, then inserts.

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
    const ally = std.heap.page_allocator;
    // Maximum diff length
    const max_d = (a.len + b.len + 1) / 2;
    const v_offset = max_d;
    const v_len = 2 * max_d;

    var v1 = try std.ArrayList(isize).initCapacity(ally, v_len);
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

    const delta: isize = @intCast(isize, a.len) - @intCast(isize, b.len);
    // If the total number of characters is odd, then the front path will
    // collide with the reverse path.
    const front = @mod(delta, @as(isize, 2)) != 0;
    // Offsets for start and end of k loop.
    // Prevents mapping of space beyond the grid.
    var k1start: isize = 0;
    var k1end: isize = 0;
    // var k2start = 0;
    // var k2end = 0;

    var d: isize = 0;

    while (d < max_d) : (d += 1) {
        // Walk the font path one step
        var k1 = -d + k1start;
        while (k1 <= d - k1end) {
            const k1_offset: usize = @intCast(usize, (@intCast(isize, v_offset) + k1));
            var x1 = if (k1 == -d or (k1 != d and v1.items[k1_offset - 1] < v1.items[k1_offset + 1])) v1.items[k1_offset + 1] else v1.items[k1_offset - 1] + 1;
            var y1 = (x1 - k1);
            // if let (Some(s1), Some(s2)) = (text1.get(x1..), text2.get(y1..)) {
            // let advance = common_prefix_bytes(s1, s2);
            // x1 += advance;
            // y1 += advance;
            // }

            v1.items[k1_offset] = x1;

            if (x1 > a.len) {
                // Ran off the right of the graph.
                k1end += 2;
            } else if (y1 > b.len) {
                // Ran off the bottom of the graph.
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
            k1 += 2;
        }
    }

    // If we haven't returned earlier, the number of diffs equals number of
    // characters, no commonality at all.
    try list.appendSlice(&.{ Edit.newDelete(0, @intCast(u32, a.len)), Edit.newInsert(0, @intCast(u32, b.len)) });
}

fn cloneUtf8Iterator(it: std.unicode.Utf8Iterator) std.unicode.Utf8Iterator {
    return std.unicode.Utf8Iterator{ .bytes = it.bytes, .i = it.i };
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
