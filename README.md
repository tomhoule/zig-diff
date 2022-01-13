# deefs

Zig diffing library heavily inspired by
[diff-match-patch](https://github.com/google/diff-match-patch) and David
Tolnay's [dissimilar](https://github.com/dtolnay/dissimilar) Rust crate.

The basic approach is [Myer's diff
algorithm](https://neil.fraser.name/writing/diff/myers.pdf) with [semantic
cleanups](https://neil.fraser.name/writing/diff/).

This project is developed and tested on Zig 0.9.
