const std = @import("std");
const testing = std.testing;
const unicode = std.unicode;

/// Returns the common prefix length in _bytes_. It also advances the iterators.
pub fn findCommonPrefix(a: []const u8, b: []const u8) usize {
    var bytesPrefix = findCommonPrefixBytes(a, b);
    if (bytesPrefix > 0 and bytesPrefix < a.len) {
        alignUtf8Backward(a, &bytesPrefix);
    }
    return bytesPrefix;
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
pub fn findCommonSuffix(a: []const u8, b: []const u8) usize {
    var bytesSuffix = findCommonSuffixBytes(a, b);
    alignUtf8Forward(a, &bytesSuffix);
    return bytesSuffix;
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

/// Returns the common suffix length in _bytes_, ignoring utf-8 character boundaries.
pub fn findCommonSuffixBytes(a: []const u8, b: []const u8) usize {
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
    // }/scii

    return i;
}

/// Returns the common suffix length in _bytes_, ignoring utf-8 character boundaries.
pub fn findCommonPrefixBytes(a: []const u8, b: []const u8) usize {
    const max = std.math.min(a.len, b.len);
    var i: u32 = 0;
    while (i < max and a[i] == b[i]) : (i += 1) {}
    return i;
}

/// Given an arbitrary index in a byte slice, return the index of the first
/// byte of the _next_ valid UTF-8 sequence.
///
/// Invariant: alignUtf8Forward(s, idx) >= idx.
pub fn alignUtf8Forward(s: []const u8, idx: *usize) void {
    while (idx.* < s.len) {
        _ = unicode.utf8ByteSequenceLength(s[idx.*]) catch {
            idx.* += 1;
            continue;
        };
        break;
    }
}

/// Given an arbitrary index in a byte slice, return the index of the first
/// byte of the valid UTF-8 sequence s[idx] is a part of.
///
/// Invariant: alignUtf8Backward(s, idx) <= idx.
pub fn alignUtf8Backward(s: []const u8, idx: *usize) void {
    if (idx.* == s.len) {
        return;
    }

    while (idx.* > 0) {
        _ = unicode.utf8ByteSequenceLength(s[idx.*]) catch {
            idx.* -= 1;
            continue;
        };
        break;
    }
}
