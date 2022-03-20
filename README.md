# `libgit2-x86_64-win32`

This is the Racket package `libgit2-x86_64-win32`, which contains the
[libgit2](https://libgit2.org) shared library built for `x86_64-win32`.

## License

The libgit2 shared library is under the license
`(`[`GPL-2.0-only`](https://spdx.org/licenses/GPL-2.0-only.html)` WITH
`[`GCC-exception-2.0`](https://spdx.org/licenses/GCC-exception-2.0.html)`)`.
The exception grants “unlimited permission to link the compiled version
of” libgit2 “into combinations with other programs, and to distribute
those combinations without any restriction coming from the use of”
libgit2: see the [COPYING](./COPYING) and
[README-libgit2.md](./README-libgit2.md) files for further details.

The [Provenance](#provenance) section of this file explains how to get
the corresponding source for the compiled version of libgit2 distributed
in this package.

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

## Provenance

The contents of this package were generated using
[Guix](https://guix.gnu.org) and a [Nix
flake](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html)
with NAR hash `sha256-dYU0Xn1VXGUrTTn337LJ797RW7rSdz95I5Z1wGYjZwU=`,
last modified on 2022-03-20T00:36:05Z. The flake is from Git commit
[`8d99f34081bcfcd141c74bf6fe967714bf58bce0`](https://github.com/LiberalArtist/native-libgit2-pkgs/commit/8d99f34081bcfcd141c74bf6fe967714bf58bce0):
if you are reading this file in a Git repository with the same structure
as
[https://github.com/LiberalArtist/native-libgit2-pkgs](https://github.com/LiberalArtist/native-libgit2-pkgs),
that commit should be part of the
[`build-scripts`](https://github.com/LiberalArtist/native-libgit2-pkgs/tree/build-scripts)
branch.

The shared library was compiled by Guix on `x86_64-linux` for
`x86_64-win32`, and this package was assembled by Guix on
`x86_64-linux`.

The included libgit2 shared library is version 1.4.2, built from the
source at
[https://github.com/libgit2/libgit2/tree/v1.4.2](https://github.com/libgit2/libgit2/tree/v1.4.2).
The Nix sha256 hash of the source is
`0xd5w2kzdafipf10sdjmrzzsi12q8rkpcafajwlnmwvrbg6ldvs5`.

The build environment used Nixpkgs from the
[`nixos-21.11`](https://github.com/NixOs/nixpkgs/tree/nixos-21.11)
branch of
[https://github.com/NixOs/nixpkgs](https://github.com/NixOs/nixpkgs),
which resolved to commit
[`2c66a7a6e036971c4847cca424125f55b9eb0b0b`](https://github.com/NixOs/nixpkgs/commit/2c66a7a6e036971c4847cca424125f55b9eb0b0b)
\(last modified 2022-03-17T04:12:11Z) with NAR hash
`sha256-Jcc+vHNDN3KDWuzGNTl3A24ICGovPneJDejiN2t57QI=`. Guix was used
from the
[`master`](https://git.savannah.gnu.org/cgit/guix.git/log/?h=master)
branch of
[https://git.savannah.gnu.org/git/guix.git](https://git.savannah.gnu.org/git/guix.git),
which resolved to commit
[`0d216771704bb93f72a0ef4d22f7af52df97b5de`](https://git.savannah.gnu.org/cgit/guix.git/tree/?id=0d216771704bb93f72a0ef4d22f7af52df97b5de).
The Guix channel was authenticated starting at commit
[`9edb3f66fd807b096b48283debdcddccfea34bad`](https://git.savannah.gnu.org/cgit/guix.git/tree/?id=9edb3f66fd807b096b48283debdcddccfea34bad)
with OpenPGP fingerprint `BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A
54FA`.

For further details, see the [provenance/](./provenance/) directory.
