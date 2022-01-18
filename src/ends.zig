const std = @import("std");
const testing = std.testing;
const unicode = std.unicode;

/// Returns the common prefix length in _bytes_. It also advances the iterators.
pub fn findCommonPrefix(a: []const u8, b: []const u8) u32 {
    // First, determine the common prefix as an optimization.
    var aIter = unicode.Utf8View.initUnchecked(a).iterator();
    var bIter = unicode.Utf8View.initUnchecked(b).iterator();
    var i: u32 = 0;
    while (aIter.nextCodepointSlice()) |aNext| {
        const bNext = bIter.nextCodepointSlice() orelse break;

        if (!std.mem.eql(u8, aNext, bNext)) {
            break;
        }

        i += @intCast(u32, aNext.len);
    }

    return i;
}

fn assertCommonPrefix(a: []const u8, b: []const u8, expectedPrefix: []const u8) !void {
    const prefixLength = findCommonPrefix(a, b);
    try testing.expectEqualSlices(u8, expectedPrefix, a[0..prefixLength]);
    try testing.expectEqualSlices(u8, expectedPrefix, b[0..prefixLength]);
}

test "ascii prefix" {
    try assertCommonPrefix("fufufufafafa", "fufufufefefe", "fufufuf");
}

test "simple multibyte prefix" {
    try assertCommonPrefix("Πλωτῖνος", "Πλάτων", "Πλ");
}

test "tricky multibyte prefix" {
    const snowman = "\u{2603}";
    const comet = "\u{2604}";
    try assertCommonPrefix(snowman, comet, "");
    try assertCommonPrefix("abba " ++ snowman, "abba " ++ comet, "abba ");
}

/// Returns the common suffix length in _bytes_.
fn findCommonSuffix(a: []const u8, b: []const u8) u32 {
    const max = @intCast(u32, std.math.min(a.len, b.len));

    if (max == 0) {
        return 0;
    }

    var i: u32 = 0;

    while (i < max and a[(a.len - 1) - i] == b[(b.len - 1) - i]) {
        i += 1;
    }

    // // "rewind" to the next utf-8 character boundary
    // while (i < (max - 1)) {
    //     _ = unicode.utf8ByteSequenceLength(a[i]) catch {
    //         i += 1;
    //         continue;
    //     };

    //     break;
    // }

    return i;
}

fn assertCommonSuffix(a: []const u8, b: []const u8, expectedSuffix: []const u8) !void {
    const suffixLength = findCommonSuffix(a, b);
    try testing.expectEqualSlices(u8, expectedSuffix, a[a.len - suffixLength ..]);
    try testing.expectEqualSlices(u8, expectedSuffix, b[b.len - suffixLength ..]);
}

test "ascii suffix" {
    try assertCommonSuffix("laurel", "hardy", "");
    try assertCommonSuffix("left", "right", "t");
    try assertCommonSuffix("", "right", "");
    try assertCommonSuffix("left", "", "");
    try assertCommonSuffix("fufufufafafafefefe", "fififofefefe", "fefefe");
}

test "ascii suffix of multibyte stringss" {
    const left = "[乀丁abcd一]";
    const right = "[一abcd丁]";
    try assertCommonSuffix(left, right, "]");
}
