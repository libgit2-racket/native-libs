native-libgit2-pkgs
=====================

This repository packages the `libgit2` native library for
the Racket package system.

These platform-specific packages are directories
at the root of this repository:

- `libgit2-x86_64-macosx`
    
- `libgit2-x86_64-linux`

- `libgit2-win32-x86_64` *(currently fails test)*

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

The `src` submodule is pinned to a specific commit of `libgit2`:
currently, the `v0.28.0` tag.
It should be updated to `v1.0.0`
[when it becomes available](https://github.com/libgit2/libgit2/issues/4960)
and the packages should be rebuilt.

**Do not modify the Racket packages directly!**
The packages are generated automatically by the `make-libgit2.rkt`
script in the root of this repository.
(It has no dependencies beyond the main Racket distribution.)
Edit the script (on a branch) to change the packages.
The script must then be run on each of the supported platforms to
compile the appropriate binaries.
Windows and Linux builds are run by AppVeyor when a commit message
begins with `BUILD`, case-insensitively.
After the build completes, download the package directory as a .zip
file from the "Artifacts" tab of each job and manually add them to the
repository. (There is probably a more elegant way to do this.)
You must run the Mac OS job yourself.
Make a new commit with all of the updated packages.

