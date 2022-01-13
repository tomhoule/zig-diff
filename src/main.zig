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
        return Edit{ .type = .Delete, .range = .{ start, end } };
    }

    pub fn newEqual(start: u32, end: u32) @This() {
        return Edit{ .type = .Delete, .range = .{ start, end } };
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

    const aFirstChar = aView.iterator().nextCodepointSlice() orelse std.debug.panic("boom", .{});
    const bFirstChar = bView.iterator().nextCodepointSlice() orelse std.debug.panic("boom", .{});

    var deleteA: Edit = Edit{ .type = .Delete, .range = .{ 0, @intCast(u32, aFirstChar.len) } };
    var insertB: Edit = Edit{ .type = .Insert, .range = .{ 0, @intCast(u32, bFirstChar.len) } };
    try list.append(deleteA);
    try list.append(insertB);
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

    try testing.expectEqualSlices(Edit, diffBuf.items, expected);
}

// ported from dtolnay/dissimilar: https://github.com/dtolnay/dissimilar/blob/master/tests/test.rs
test "diff emojis with longer string" {
    _ = testing.allocator;

    _ = "[乀丁abcd一]".*;
    _ = "[一abcd丁]".*;

    // let d = diff(a, b);
    // assert_eq!(
    //     d,
    //     vec![
    //         Chunk::Equal("["),
    //         Chunk::Delete("乀丁"),
    //         Chunk::Insert("一"),
    //         Chunk::Equal("abcd"),
    //         Chunk::Delete("一"),
    //         Chunk::Insert("丁"),
    //         Chunk::Equal("]"),
    //     ]
    //     // Unicode snowman and unicode comet have the same first two bytes. A
    //     // byte-based diff would produce a 2-byte Equal followed by 1-byte Delete
    //     // and Insert.
    //     const snowman = "\u{2603}";
    //     const comet = "\u{2604}";
    //     te
    //     assert_eq!(snowman.as_bytes()[..2], comet.as_bytes()[..2]);

    //     let d = diff(snowman, comet);
    //     assert_eq!(d, vec![Chunk::Delete(snowman), Chunk::Insert(comet)]);
}

fn testUtf8Diff(a: []const u8, b: []const u8, expected: []const u8) anyerror!void {
    const ally = std.testing.allocator;
    var buf = std.ArrayList(Edit).init(ally);
    try diff(a, b, buf);

    // var expectedBuf = std.ArrayList(Edit).init(ally);
    const expectedUtf8 = try unicode.Utf8View.init(expected).iterator();
    var idx = 0;

    while (idx < expected.len) {
        const char = expectedUtf8.nextCodepointSlice();
        switch (char) {
            "$" => {
                switch (expectedUtf8.nextCodepointSlice()) {
                    "+" => {
                        unreachable;
                    },
                    "-" => {
                        unreachable;
                    },
                    "=" => {
                        unreachable;
                    },
                }
            },
            else => {
                idx += char.len;
            },
        }
    }
}

test "compileDiffSpec works" {
    const ds1 = compileDiffSpec("$=abcd");
    const ds1_exp: []const Edit = &.{Edit{ .type = .Equal, .range = .{ 0, 4 } }};
    try testing.expectEqualSlices(Edit, ds1_exp, ds1);

    const ds2 = compileDiffSpec("$-abcd$+efgh");
    const ds2_exp: []const Edit = &.{ Edit{ .type = .Delete, .range = .{ 0, 4 } }, Edit{ .type = .Insert, .range = .{ 0, 4 } } };
    try testing.expectEqualSlices(Edit, ds2_exp, ds2);

    const ds3 = compileDiffSpec("$-abcd$+efgh$= $-aiue$+ζιγ");
    const ds3_exp: []const Edit = &.{ Edit{ .type = .Delete, .range = .{ 0, 4 } }, Edit{ .type = .Insert, .range = .{ 0, 4 } }, Edit{ .type = .Equal, .range = .{ 4, 5 } }, Edit{ .type = .Delete, .range = .{ 5, 9 } }, Edit{ .type = .Insert, .range = .{ 5, 11 } } };
    try testing.expectEqualSlices(Edit, ds3_exp, ds3);
}

const DiffSpec = struct {
    diff: []const Edit,
    a: []u8,
    b: []u8,
};

fn compileDiffSpec(comptime expected: []const u8) []const Edit {
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

        return edits;
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
