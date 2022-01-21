# zig-diff

Zig text diffing library inspired and partially based on
[diff-match-patch](https://github.com/google/diff-match-patch) and David
Tolnay's [dissimilar](https://github.com/dtolnay/dissimilar) Rust crate.

The basic approach is [Myer's diff
algorithm](https://neil.fraser.name/writing/diff/myers.pdf). Both bytewise
diffing and utf-8 aware diffing are supported.

**This project should be considered alpha quality**, the test suite isn't
particularly exhaustive and there are a number of improvements to be made for
more readable diffs. The public API will change.

The diff-match-patch library and dissimilar implement [semantic
cleanups](https://neil.fraser.name/writing/diff/) for more human-friendly,
readable diffs. This is something we will implement in this library, but we
don't have it yet.

This project is developed and tested on Zig 0.9.
