native-libgit2-pkgs
=====================

This repository packages the `libgit2` native library for
the Racket package system.

These platform-specific packages are directories
at the root of this repository:

    - `libgit2-x86_64-macosx`
    
    - `libgit2-x86_64-linux`

    - `libgit2-i386-linux`

    - `libgit2-win32-x86_64`

    - `libgit2-win32-i386`

The root of this repository is not itself a Racket package:
it contains support for building the packages listed above.

The `libgit2` source is included as a Git submodule at `src`.
The files `AUTHORS` and `COPYING` are drawn from upstream `libgit2`:
note that the `libgit2` maintainers are not responsible for this
repository in any way.
(I am, however, grateful for help some of them have given me
with the build process.)

Build Instructions
------------------

*These instructions are tentative and subject to change!*

Development should always be done on a branch.
Only merge into `master` when all of the platform-specific packages
are in sync.

Be sure to check out the `src` submodule,
either with `git clone --recurse-submodules`
or by running `git submodule init && git submodule update`
after you have cloned the repository.

The `src` submodule is pinned to a specific commit of `libgit2`.
Currently, it is a pre-release of version 0.28.
It should be updated to the `v0.28.0` tag when it becomes available
(soon!) and the packages should be rebuilt.



