# Native `libgit2` packages for Racket: `build-scripts`

This repository packages the [libgit2](https://libgit2.org) shared
library for the Racket package system.

The scripts contained in this branch (`build-scripts`) are used to
compile libgit2 for all supported platforms and to pack the binaries
into platform-specific Racket packages. Each generated package is
committed to its own “orphan” branch of this repository: the `main`
branch serves as a navigational aide.

The build scripts are structured as a [Nix
flake](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html)
that generates [Guix](https://guix.gnu.org) package definitions.

This [README.md](./README.md) file itself is also generated
programmatically: see [Changing Things](#changing-things) for
instructions on editing and regenerating it.

**Quick Start:** Try `nix run` or `make show`. See [Running the
Build](#running-the-build) for more.

  
> [Prerequisites](#prerequisites)  
>> [Recommended Setup](#recommended-setup)  
>> [Continuous Integration](#continuous-integration)  
  
> [Running the Build](#running-the-build)  
  
> [Changing Things](#changing-things)  
  
> [License](#license)  

## Prerequisites

* Compiling libgit2 for Apple platforms currently requires an
  `x86_64-macosx` build machine with the
  [Nix](https://nixos.org/manual/nix/stable/introduction.html) package
  manager installed.

  * It should be fairly trivial to support building on `aarch64-macosx`,
    with only the cost of supporting another possible configuration.

  * It would be very valuable to remove the requirement for an Apple
    build machine, but this seems challenging. Nix does not currently
    support cross-compiling for Apple hosts (in the Autoconf sense) from
    non-Apple systems, and Guix neither runs on Darwin nor has the
    requisite cross-compilation toolchain packages.

    In the mean time, for development without an Apple machine, the Nix
    flake supports skipping the compilation for Apple hosts, and you can
    do everything else with just an `x86_64-linux` build machine (which
    could be a VM) with both Nix and Guix installed. See [Running the
    Build](#running-the-build) for instructions.

* Compiling libgit2 for non-Apple platforms and assembling the Racket
  packages (including for Apple platforms) requires an `x86_64-linux`
  build machine with the [Guix](https://guix.gnu.org) package manager
  installed. You probably need to have Nix installed on this machine,
  too.

  * It should be fairly trivial to support building on other
    architectures Guix supports, with only the cost of supporting
    another possible configuration.

  * Much of the complexity overall is due to using both Guix and Nix,
    rather than one or the other. Using only Guix would require solving
    the Apple cross-compilation challenge discussed above. Using only
    Nix seems like it should be possible, but linking against older
    versions of Glibc with Guix seemed easier than using Nix—or at least
    Guix seemed to involve more parentheses.

### Recommended Setup

For maximum convenience, work on an `x86_64-linux` machine (a VM is
fine) with both Nix and Guix installed. For the Apple-specific
compilation, set up an `x86_64-macosx` machine with Nix installed that
you can access via SSH as a Nix “remote builder”: see the [Nix
Manual](https://nixos.org/manual/nix/stable/advanced-topics/distributed-builds.html)
and [wiki article](https://nixos.wiki/wiki/Distributed_build) for
details.

This will let you do everything discussed under [Running the
Build](#running-the-build) transparently, with Nix automatically
delegating the Apple-specific steps to run on the remote machine.

> Setting Nix up for distributed builds, especially on the Apple side, was
> more complicated than I would have hoped: I wish I’d taken more detailed
> notes as I went through the process. In particular, note that you
> probably need `root` on the `x86_64-linux` machine to be able to SSH
> non-interactively (i.e. without a passphrase) into an account on the
> `x86_64-macosx` machine that:
* > Has the `nix` command `PATH` of “non-interactive login shells”; and

* > Is listed under `trusted-users` in the Mac’s `"/etc/nix/nix.conf"`
  > (but see
  > [https://github.com/NixOS/nix/pull/3921](https://github.com/NixOS/nix/pull/3921)

### Continuous Integration

Continuous integration remains as an area for future work.

In principle, the general approach would likely be a pipeline that:

* On an `x86_64-macosx` runner, does the equivalent of `nix build`, then
  uses `nix copy` to export the resulting Nix store item as an artifact;

* On an `x86_64-linux` runner, uses `nix copy` to import the Nix store
  item from the previous step, then builds the Racket packages as
  described under [Running the Build](#running-the-build);

* On various runners, potentially in parallel, tests the built runners
  as desired (or maybe this is better left for the CI of the Racket
  `libgit2` package itself); and

* Finally, exports the built packages in some useful way, perhaps even
  committing them to the appropriate branches.

## Running the Build

Write things here ...

## Changing Things

Write things here ...

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
