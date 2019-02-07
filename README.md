native-libgit2-pkgs
=====================

This repository packages the `libgit2` native library for
the Racket package system.

Currently, a pre-release of version 2.28 is packaged for `x86_64-macosx`.

The `libgit2` source is included as a Git submodule at `src`.
The files `AUTHORS` and `COPYING` are drawn from upstream `libgit2`:
note that the `libgit2` maintainers are not responsible for this
repository in any way.
(I am, however, grateful for help some of them have given me
with the build process.)

The `libgit2-x86_64-macosx` directory contains the actual Racket package.

