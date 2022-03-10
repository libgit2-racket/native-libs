# `libgit2-i386-win32`

This is the Racket package `libgit2-i386-win32`, which contains the
[libgit2](https://libgit2.org) shared library built for `i386-win32`.

## Provenance

The contents of this package were generated using a [Nix
flake](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html)
with NAR hash `sha256-QIpHRAK5I4a/7rPlrclR6cgaJet8lOXRbov6NDFyMDs=`,
last modified on 2022-03-10T16:44:21Z. The flake is from Git commit
37f6d2229968c755617dcde4cb6e53f1274de8cc: if you are reading this file
in a Git repository with the same structure as
[https://github.com/LiberalArtist/native-libgit2-pkgs](https://github.com/LiberalArtist/native-libgit2-pkgs),
that commit should be part of the
[`build-scripts`](https://github.com/LiberalArtist/native-libgit2-pkgs/tree/build-scripts)
branch.

The flake was built on `x86_64-unknown-linux-gnu` for `i386-win32`.

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
libgit2: see the [COPYING](./COPYING) and
[README-libgit2.md](./README-libgit2.md) files for further details. The
[Provenance](#provenance) section of this file explains how to get the
corresponding source for the compiled version of libgit2 distributed in
this package.

\(Note that [AUTHORS](./AUTHORS) and similar files in this repository
are drawn from upstream libgit2, but the libgit2 maintainers are not
responsible for this repository in any way. (I am, however, grateful for
advice some of them have given.))

The build scripts and other miscellaneous files added as part of the
Racket packaging are distributed under the
[`Apache-2.0`](https://spdx.org/licenses/Apache-2.0.html) license or the
[`MIT`](https://spdx.org/licenses/MIT.html) license, at your option
(i.e. the same license as Racket). By making a contribution, you are
agreeing that your contribution is licensed under the `Apache-2.0`
license and the `MIT` license.
