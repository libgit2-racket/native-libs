# `libgit2-native-libs`

This is the Racket package `libgit2-native-libs`, a meta-package
distributing the native [libgit2](https://libgit2.org) shared library
via platform-specific dependencies in the Racket package system.

Currently, the pre-built library is provided for the following
platforms:

* `x86_64-linux` (package:
  [`libgit2-x86_64-linux`](https://github.com/LiberalArtist/native-libgit2-pkgs/tree/x86_64-linux))

* `aarch64-macosx` (package:
  [`libgit2-aarch64-macosx`](https://github.com/LiberalArtist/native-libgit2-pkgs/tree/aarch64-macosx))

* `x86_64-macosx` (package:
  [`libgit2-x86_64-macosx`](https://github.com/LiberalArtist/native-libgit2-pkgs/tree/x86_64-macosx))

* `i386-win32` (package:
  [`libgit2-i386-win32`](https://github.com/LiberalArtist/native-libgit2-pkgs/tree/i386-win32))

* `x86_64-win32` (package:
  [`libgit2-x86_64-win32`](https://github.com/LiberalArtist/native-libgit2-pkgs/tree/x86_64-win32))

If you are reading this file in a Git repository with the same structure
as
[https://github.com/LiberalArtist/native-libgit2-pkgs](https://github.com/LiberalArtist/native-libgit2-pkgs),
you can find the platform-specific packages on sibling branches of this
one. The [`main`](https://github.com/LiberalArtist/native-libgit2-pkgs)
branch may help you find the relevant branches and commits.

## Provenance

These contents of these packages, including this meta-package, were
generated using a [Nix
flake](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html)
with NAR hash `sha256-Xcs3KC7hcuuXo7Ixc0SzIAz7OXOKuOvtcOh4oDJBiR8=`,
last modified on 2022-03-10T21:03:10Z. The flake is from Git commit
[`4976f01fb9ed1afddfc2150735cdad278f3c36b9`](https://github.com/LiberalArtist/native-libgit2-pkgs/commit/4976f01fb9ed1afddfc2150735cdad278f3c36b9):
if you are reading this file in a Git repository with the same structure
as
[https://github.com/LiberalArtist/native-libgit2-pkgs](https://github.com/LiberalArtist/native-libgit2-pkgs),
that commit should be part of the
[`build-scripts`](https://github.com/LiberalArtist/native-libgit2-pkgs/tree/build-scripts)
branch.

The flake was built on `x86_64-unknown-linux-gnu` to generate this
package, libgit2-native-libs.

The included libgit2 shared library is version 1.4.2, built from the
source at
[https://github.com/libgit2/libgit2/tree/v1.4.2](https://github.com/libgit2/libgit2/tree/v1.4.2).
The Nix sha256 hash of the source is
`0xd5w2kzdafipf10sdjmrzzsi12q8rkpcafajwlnmwvrbg6ldvs5`.

The build environment used the
[`nixos-21.11`](https://github.com/NixOs/nixpkgs/tree/nixos-21.11)
branch of
[https://github.com/NixOs/nixpkgs](https://github.com/NixOs/nixpkgs),
which resolved to commit
[`2ebb6c1e5ae402ba35cca5eec58385e5f1adea04`](https://github.com/NixOs/nixpkgs/commit/2ebb6c1e5ae402ba35cca5eec58385e5f1adea04)
\(last modified 2022-03-06T17:37:36Z) with NAR hash
`sha256-ZHljmNlt19nSm0Mz8fx6QEhddKUkU4hhwFmfNmGn+EY=`.

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
