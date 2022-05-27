# Native `libgit2` packages for Racket: `main`

This repository packages the [libgit2](https://libgit2.org) shared library for
the Racket package system.

Platform-specific packages and the [`libgit2-native-libs`](./native-libs)
meta-package reside on orphan branches of this repository. The
[`build-scripts`](./build-scripts) branch contains the code used to generate
the packages.

This `main` branch serves primarily as a navigational aide.

Rather than using these packages directly, you should probably instead use the
Racket [`libgit2`](https://pkgs.racket-lang.org/package/libgit2) package,
which provides Racket bindings to libgit2.

## License

The libgit2 shared library is under the license
`(`[`GPL-2.0-only`](https://spdx.org/licenses/GPL-2.0-only.html)` WITH
`[`GCC-exception-2.0`](https://spdx.org/licenses/GCC-exception-2.0.html)`)`.
The exception grants “unlimited permission to link the compiled version
of” libgit2 “into combinations with other programs, and to distribute
those combinations without any restriction coming from the use of”
libgit2: see the platform-specific packages for further details.

The build scripts and other miscellaneous files added as part of the
Racket packaging are distributed under the
[`Apache-2.0`](https://spdx.org/licenses/Apache-2.0.html) license or the
[`MIT`](https://spdx.org/licenses/MIT.html) license, at your option
(i.e. the same license as Racket). By making a contribution, you are
agreeing that your contribution is licensed under the `Apache-2.0`
license and the `MIT` license.
