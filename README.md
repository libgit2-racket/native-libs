# Native `libgit2` packages for Racket: `build-scripts`

This repository packages the [libgit2](https://libgit2.org) shared
library for the Racket package system.

The scripts contained qin this branch (`build-scripts`) are used to
compile libgit2 for all supported platforms and to pack the binaries
into platform-specific Racket packages, plus a meta-package that
encapsulates the platform-conditional dependencies. Each generated
package is committed to its own “orphan” branch of this repository: the
`main` branch serves as a navigational aide.

The build scripts are structured as a [Nix
flake](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html)
that generates [Guix](https://guix.gnu.org) package definitions.

This [README.md](./README.md) file itself is also generated
programmatically: see [Changing Things](#changing-things) for
instructions on editing and regenerating it.

**Quick Start:** Try `nix run`, `make show`, or—without even needing to
clone this repository!—`nix flake show
github:LiberalArtist/native-libgit2-pkgs/build-scripts`. See [Running
the Build](#running-the-build) for more.

**Contents:**

  
> [Prerequisites](#prerequisites)  
>> [Recommended Setup](#recommended-setup)  
>> [Continuous Integration](#continuous-integration)  
  
> [Running the Build](#running-the-build)  
>> [Alternate Command Spellings](#alternate-command-spellings)  
  
> [Changing Things](#changing-things)  
>> [Update Lockfiles](#update-lockfiles)  
  
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

**Important!** Git tree dirty ...

In principle, you don’t even need a Git clone of this repository to
generate the Racket packages. Assuming you have the [recommended
setup](#recommended-setup), running the command:

  `nix run github:LiberalArtist/native-libgit2-pkgs/build-scripts`

will compile libgit2 and generate the Racket packages for all platforms.
In practice, you probably will have a Git clone, so you’ll likely prefer
to use:

  `nix run .#guix-build`

or, even shorter, just `nix run` (see [Alternate Command
Spellings](#alternate-command-spellings)).

To create e.g. `"tmp-guix-output"` as a symlink to the build result
(which resides in the immutable Guix store), run:

  `NIX_RUN_GUIX_BUILD_ROOT=tmp-guix-output nix run`

More generally, `nix run .#guix-build` runs [`guix
build`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-build.html)
in an environment respecting [channels.scm](./channels.scm) \(see
[Update Lockfiles](#update-lockfiles)) and with the Guix code generated
by Nix available. You can call `nix run .#guix-build --` with arguments
to pass to `guix build` instead of the default \(`--keep-failed
racket-libgit2-omnibus`), and defining `NIX_RUN_GUIX_BUILD_ROOT` causes
the
[`--root=`](https://guix.gnu.org/manual/devel/en/html_node/Additional-Build-Options.html#index-GC-roots_002c-adding)
flag to be added.

Similarly, `nix run .#guix-show` provides an interface to [`guix
show`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-package.html)
with a default argument of `racket-libgit2-omnibus` if no arguments are
provided. (This command does not consult `NIX_RUN_GUIX_BUILD_ROOT`.)

Most generally of all, `nix run .#time-machine --` provides an analagous
to [`guix  time-machine
--`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-time_002dmachine.html)
\(with no implicit arguments\). Arguments supplied to `nix run
.#time-machine --` are passed directly to the `guix` command in the
[appropriate environment](#update-lockfiles). This command is used to
implement `nix run .#guix-build` and `nix run .#guix-show`.

For development without access to an Apple machine, there are also
variants of these commands suffixed with `-sans-apple` (e.g. `nix run
.#guix-build-sans-apple`) that skip the packages for Apple platforms
that we [currently can’t cross-compile](#prerequisites) from non-Apple
build machines.

Running `make show` will print a complete listing of available commands,
along with other useful metadata. (In contrast, the other `make` targets
are used for [updating the contents of this
repository](#update-lockfiles), rather than for compiling libgit2 or
generating Racket packages.) The items listed under “apps” are commands
for `nix run`.

The above commands invoked via `nix run` ultimately run Guix, so they
only work on [platforms for which Guix is
available](https://guix.gnu.org/manual/devel/en/html_node/GNU-Distribution.html)
(i.e. GNU/Linux).

Instead, the command `nix build .#guix-with-apple` (or just `nix build`)
compiles libgit2 for all Apple platforms (c.f. `nix build
.#guix-sans-apple`) and generates the files that will become the input
to the Guix stage of the build. The other items listed by `make show`
under “packages” can also be used with `nix build` to build more
granular pieces. All of the commands using `nix build` can be run on any
platform Nix supports, including Apple machines.

Note that `nix build` will create `"result"` as a symlink to the build
result (which lives in the immutable Nix store) unless `--no-link` or
`-o`/`--out-link` is used.

### Alternate Command Spellings

In commands like:

  `nix run .#guix-build`

the `.#guix-build` subform is a Nix
[“installable”](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix.html#installables):
specifically, a “flake output reference”. These can be written in many
equivalent ways, but, in brief:

* The `.` portion can be replaced by a different form of [flake
  reference](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html#flake-references),
  e.g. `github:LiberalArtist/native-libgit2-pkgs/build-scripts` \(but
  note that this example would build the commit from the specified Git
  repository and branch, rather than your local working tree!);

* The “fragment” (the portion beginning with `#`) can be ommitted if it
  refers to the `defaultApp` or `defaultPackage` reported by `make show`
  (that’s `#guix-build` and `#guix-with-apple`); and

* A command of the form `nix ⟨cmd⟩ .`—i.e., without any fragment or
  arguments—is equivalent to `nix ⟨cmd⟩`.

## Changing Things

### Update Lockfiles

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
