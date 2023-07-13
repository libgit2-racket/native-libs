#lang scribble/base
@; SPDX-License-Identifier: (Apache-2.0 OR MIT)

@title{Native @tt{libgit2} packages for Racket: @tt{build-scripts}}

@(require "doc-utils.rkt"
          scribble/manual)

@(define (nix-manual page)
   (++ "https://nixos.org/manual/nix/stable/" page))
@(define (guix-manual page)
   (++ "https://guix.gnu.org/manual/devel/en/html_node/" page))

This repository packages the
@hyperlink["https://libgit2.org"]{libgit2} shared library
for the Racket package system.

The scripts contained in this branch (@tt{build-scripts})
are used to compile libgit2 for all supported platforms and
to pack the binaries into platform-specific Racket packages,
plus a meta-package that encapsulates the
platform-conditional dependencies. Each generated package is
committed to its own ``orphan'' branch of this repository:
the @tt{main} branch serves as a navigational aide.

The build scripts are structured as a
@hyperlink[(nix-manual "command-ref/new-cli/nix3-flake.html")]{
 Nix flake} that generates
@hyperlink["https://guix.gnu.org"]{Guix} package
definitions.

This @rel-link{README.md} file itself is also generated
programmatically: see @secref{edit-readme} for
instructions on editing and regenerating it.

@bold{Quick Start:} Try @exec{nix run}, @exec{make show},
or---without even needing to clone this
repository!---@exec{nix flake show
 github:libgit2-racket/native-libs/build-scripts}.
See @secref{run} for more.

@bold{Contents:}

@table-of-contents[]

@; ---------------------------------------------------------------------------------------------------
@; ---------------------------------------------------------------------------------------------------
@md-section{Prerequisites}

@itemlist[
 #:style 'ordered
 @item{Compiling libgit2 for Apple platforms currently requires
  an @tt{x86_64-macosx} build machine with the
  @hyperlink[(nix-manual "introduction.html")]{Nix}
  package manager installed.

  @itemlist[@item{It should be fairly trivial to support
               building on @tt{aarch64-macosx}, with only the cost of
               supporting another possible configuration.}

            @item{It would be very valuable to remove the
               requirement for an Apple build machine, but this seems
               challenging. Nix does not currently support cross-compiling
               for Apple hosts (in the Autoconf sense) from non-Apple
               systems, and Guix neither runs on Darwin nor has the
               requisite cross-compilation toolchain packages.

               In the mean time, for development without an Apple machine,
               the Nix flake supports skipping the compilation for Apple
               hosts, and you can do everything else with just an
               @tt{x86_64-linux} build machine (which could be a VM) with
               both Nix and Guix installed. See @secref{run}
               for instructions.}
            ]}
 @item{Compiling libgit2 for non-Apple platforms and
  assembling the Racket packages (including for Apple
  platforms) requires an @tt{x86_64-linux} build machine with
  the @hyperlink["https://guix.gnu.org"]{Guix} package manager
  installed. You probably need to have Nix installed on this machine, too.

  @itemlist[@item{It should be fairly trivial to support
               building on other architectures Guix supports, with only the
               cost of supporting another possible configuration.}

            @item{Much of the complexity overall is due to
               using both Guix and Nix, rather than one or the other. Using
               only Guix would require solving the Apple cross-compilation
               challenge discussed above. Using only Nix seems like it
               should be possible, but linking against older versions of
               Glibc with Guix seemed easier than using Nix---or at least
               Guix seemed to involve more parentheses.}
            ]}]

@md-section[subsection #:tag "recommended-setup"]{Recommended Setup}

For maximum convenience, work on an @tt{x86_64-linux}
machine (a VM is fine) with both Nix and Guix installed. For
the Apple-specific compilation, set up an @tt{x86_64-macosx}
machine with Nix installed that you can access via SSH as a
Nix ``remote builder'': see the
@hyperlink[@nix-manual{advanced-topics/distributed-builds.html}]{Nix Manual}
and @hyperlink["https://nixos.wiki/wiki/Distributed_build"]{wiki article}
for details.

This will let you do everything discussed under
@secref{run} transparently, with Nix
automatically delegating the Apple-specific steps to run on
the remote machine.

@margin-note{Setting Nix up for distributed builds,
 especially on the Apple side, was more complicated than I
 would have hoped: I wish I'd taken more detailed notes as I
 went through the process. In particular, note that you
 probably need @tt{root} on the @tt{x86_64-linux} machine to
 be able to SSH non-interactively (i.e. without a passphrase)
 into an account on the @tt{x86_64-macosx} machine that:

 @itemlist[@item{Has the @exec{nix} command in the @envvar{PATH} of
             ``non-interactive login shells''; and}

           @item{Is listed under @tt{trusted-users} in the
             Mac's @filepath{/etc/nix/nix.conf} (but see
             @url{https://github.com/NixOS/nix/pull/3921}}]
}

@md-section[subsection]{Continuous Integration}

Continuous integration remains as an area for future work.

In principle, the general approach would likely be a pipeline that:
@itemlist[@item{On an @tt{x86_64-macosx} runner, does the
           equivalent of @exec{nix build}, then uses @exec{nix copy} to
           export the resulting Nix store item as an artifact;}
          @item{On an @tt{x86_64-linux} runner, uses
           @exec{nix copy} to import the Nix store item from the
           previous step, then builds the Racket packages as described
           under @secref{run};}
          @item{On various runners, potentially in
           parallel, tests the built Racket packages as desired (or maybe this
           is better left for the CI of the Racket @tt{libgit2} package
           itself); and}
          @item{Finally, exports the built packages in some
           useful way, perhaps even committing them to the appropriate
           branches.}]

@; ---------------------------------------------------------------------------------------------------
@; ---------------------------------------------------------------------------------------------------
@md-section[#:tag "run"]{Running the Build}

@margin-note{
 @bold{Important!} If you see a message like
 ``@elem[#:style 'tt]{@literal{warning: Git tree '⟨path⟩' is dirty}}'',
 your Git checkout has uncommitted changes. That's fine (indeed, useful)
 for development, but please do not commit generated Racket packages
 built from a ``dirty'' tree. The generated Racket packages embed the
 specific commit from which they were built so that they can be reproduced,
 but it isn't possible to reproduce miscellaneous uncommitted changes.
 So, first commit your changes to this branch, then run a clean build
 and commit the generated packages to their respective branches.
}

In principle, you don't even need a Git clone of this
repository to generate the Racket packages. Assuming you
have the @seclink["recommended-setup"]{recommended setup},
running the command:
@commandline{nix run github:libgit2-racket/native-libs/build-scripts}
will compile libgit2 and generate the Racket packages for
all platforms. In practice, you probably will have a Git
clone, so you'll likely prefer to use:
@commandline{nix run .#guix-build}
or, even shorter, just @exec{nix run} (see @secref{flake-ref-syntax}).

To create e.g@._ @filepath{tmp-guix-output} as a symlink to
the build result (which resides in the immutable Guix
store), run:
@commandline{NIX_RUN_GUIX_BUILD_ROOT=tmp-guix-output nix run}

More generally, @exec{nix run .#guix-build} runs
@hyperlink[@guix-manual{Invoking-guix-build.html}]{@exec{guix build}}
in an environment respecting @rel-link{channels.scm}
(see @secref{edit-lockfiles}) and with the Guix code
generated by Nix available.
You can call @exec{nix run .#guix-build --} with arguments
to pass to @exec{guix build} instead of the default
(@exec{--keep-failed racket-libgit2-omnibus}),
and defining @envvar{NIX_RUN_GUIX_BUILD_ROOT} causes the
@hyperlink[@guix-manual{Additional-Build-Options.html#index-GC-roots_002c-adding}]{@DFlag{root=}}
flag to be added.

Similarly, @exec{nix run .#guix-show} provides an interface to
@; FIXME upstream: no anchor for `guix package --show`/`guix show`
@hyperlink[@guix-manual{Invoking-guix-package.html}]{@exec{guix show}}
with a default argument of @tt{racket-libgit2-omnibus} if no
arguments are provided. (This command does not consult
@envvar{NIX_RUN_GUIX_BUILD_ROOT}.)

Most generally of all, @exec{nix run .#time-machine --}
provides an analagous to
@hyperlink[@guix-manual{Invoking-guix-time_002dmachine.html}]{@exec{guix  time-machine --}}
(with no implicit arguments). Arguments supplied to
@exec{nix run .#time-machine --} are passed directly to the
@exec{guix} command in the
@seclink["edit-lockfiles"]{appropriate environment}. This
command is used to implement @exec{nix run .#guix-build} and
@exec{nix run .#guix-show}.

For development without access to an Apple machine, there
are also variants of these commands suffixed with
@tt{-sans-apple} (e.g@._ @exec{nix run .#guix-build-sans-apple})
that skip the packages for Apple
platforms that we @seclink["Prerequisites"]{currently can't cross-compile}
from non-Apple build machines.

Running @exec{make show} will print a complete listing of
available commands, along with other useful metadata. (In
contrast, the other @exec{make} targets are used for
@seclink["edit-lockfiles"]{updating the contents of this repository},
rather than for compiling libgit2 or generating
Racket packages.) The items listed under ``apps'' are
commands for @exec{nix run}.

The above commands invoked via @exec{nix run} ultimately run
Guix, so they only work on
@hyperlink[@guix-manual{GNU-Distribution.html}]{platforms
 for which Guix is available} (i.e@._ GNU/Linux).

Instead, the command @exec{nix build .#guix-with-apple} (or
just @exec{nix build}) compiles libgit2 for all Apple
platforms (c.f@._ @exec{nix build .#guix-sans-apple}) and
generates the files that will become the input to the Guix
stage of the build. The other items listed by @exec{make
 show} under ``packages'' can also be used with @exec{nix
 build} to build more granular pieces. All of the commands
using @exec{nix build} can be run on any platform Nix
supports, including Apple machines.

Note that @exec{nix build} will create @filepath{result} as
a symlink to the build result (which lives in the immutable Nix store)
unless @DFlag{no-link} or @Flag{o}/@DFlag{out-link} is used.


@md-section[subsection #:tag "flake-ref-syntax"]{Alternate Command Spellings}

In commands like:
@commandline{nix run .#guix-build}
the @tt{.#guix-build} subform is a Nix
@hyperlink[@nix-manual{command-ref/new-cli/nix.html#installables}]{``installable''}:
specifically, a ``flake output reference''. These can be
written in many equivalent ways, but, in brief:

@itemlist[
 @item{The @litchar{.} portion can be replaced by a
  different form of
  @hyperlink[@nix-manual{command-ref/new-cli/nix3-flake.html#flake-references}]{flake reference},
  e.g@._ @litchar{github:libgit2-racket/native-libs/build-scripts}
  (but note that this example would build the commit from the
  specified Git repository and branch, rather than your local
  working tree!);}
 @item{The ``fragment'' (the portion beginning with
  @litchar{#}) can be omitted if it refers to the
  @tt{defaultApp} or @tt{defaultPackage} reported by
  @exec{make show} (that's @litchar{#guix-build} and
  @litchar{#guix-with-apple}); and}
 @item{A command of the form @exec{nix ⟨cmd⟩ .}---i.e.,
  without any fragment or arguments---is equivalent to
  @exec{nix ⟨cmd⟩}.}]

@; ---------------------------------------------------------------------------------------------------
@; ---------------------------------------------------------------------------------------------------
@md-section[#:tag "edit"]{Changing Things}

This branch is organized to (hopefully!) make routine
updates easy without needing much knowledge of Nix or Guix.

Overall, this branch is organized in layers:
@itemlist[
 #:style 'ordered
 @item{The @rel-link{nix/} directory and top-level
  @filepath{.nix} files contain Nix expressions.}
 @item{The @rel-link{guix/} directory contains Guile modules
  and auxiliary files to be added to the Guile load path when
  running Guix. Many of the Guile files will not run
  successfully and may not even compile without additional
  modules and other files added by Nix. (The top-level
  @filepath{.scm} files also contain fragments of Guile, as
  opposed to Racket.)}
 @item{The @rel-link{guix/scripts/} directory contains
  Racket files. They are primarily used via Guix, but, in
  contrast to the Guile code, they can run without any
  generated files, e.g@._ to generate this
  @rel-link{README.md} file.}]

When editing Nix files, use
@hyperlink["https://github.com/serokell/nixfmt"]{@exec{nixfmt}}
to format them consistently.

Specific kinds of changes are explained in the subsections below.

@md-section[subsection #:tag "edit-lockfiles"]{Updating Lockfiles}

The files @rel-link{flake.lock} and @rel-link{channels.scm}
control the specific versions used of the Nix and Guix
package repositories, respectively. It should be safe to
update them routinely by running @exec{make update}.

@md-section[subsection #:tag "edit-version"]{Updating libgit2 and Package Versions}

The file @rel-link{version.nix} specifies the version of the
libgit2 shared library to build as well as the
@; FIXME set-external-root-url doesn't work with scribble/markdown-render
@tech[#:doc '(lib "pkg/scribblings/pkg.scrbl")]{version} (in the sense of
@secref["Package_Concepts" #:doc '(lib "pkg/scribblings/pkg.scrbl")])
of the generated Racket packages.

To support breaking changes, the @tt{breakingChangeLabel},
if it is not @racket[""], is incorporated into the names of
the generated packages and corresponding Git branches, with
hyphens managed automatically. For example, if
@tt{breakingChangeLabel} were @racket["luftschloss"], a
package would be generated called
@tt{libgit2-luftschloss-native-libs} that would expect to
live on the @tt{luftschloss-native-libs} branch.

@md-section[subsection]{Changing @exec{configure} Flags}

Most flags for libgit2's @exec{configure} script are
specified in @rel-link{nix/flags.nix}. Windows-specific
flags are instead specified in
@rel-link{guix/platforms.scm}. As a special case, the use of
@tt{-DDEPRECATE_HARD=ON} is controlled by the value of
@tt{deprecateHard} in @rel-link{version.nix} (see
@secref{edit-version}).

@md-section[subsection]{Changing the Nixpkgs Branch}

While Guix follows a ``rolling release'' model,
@hyperlink["https://github.com/NixOS/nixpkgs"]{Nixpkgs} has
semiannual stable releases, which are implemented as
branches of the Nixpkgs Git repository. The @tt{inputs}
definition at the top of @rel-link{flake.nix} specifies
which release we are using. It is important to note that the
Nixpkgs release used effectively determines
@tt{MACOSX_VERSION_MIN}: if changing the Nixpkgs branch
would drop support for any versions, it would necessitate a
new @tt{breakingChangeLabel} (see @secref{"edit-version"}).

@md-section[subsection #:tag "edit-glibc"]{Changing the Glibc Version}

The file @rel-link{guix/old-stable-libc.scm} defines the GNU
C Library version we link against for GNU/Linux systems,
which is the primary constraint on supporting old distributions.

@md-section[subsection]{Adding or Changing Platforms}

To generate platforms for an additional non-Apple platform,
add it to the list in @rel-link{guix/platforms.scm}.

The Apple platforms are instead specified in
@rel-link{nix/platforms.nix}, which also determines the
supported platforms for running the build (see @secref{Prerequisites}).
This part is more confusing than one might hope.

@md-section[subsection #:tag "edit-readme"]{Editing @filepath{README.md} Files}

This @rel-link{README.md} file itself is generated from the
Scribble source in @rel-link{guix/scripts/self-readme.scrbl}:
run @exec{make} to regenerate it. Running @exec{make update}
(see @secref{edit-lockfiles}) also regenerates this file.

Similarly, the Scribble files
@rel-link{guix/scripts/platform-readme.scrbl} and
@rel-link{guix/scripts/meta-readme.scrbl} are used in the
Guix build to generate @filepath{README.md} for the
generated Racket packages.

@md-section[subsection #:tag "edit-info-rkt"]{Editing @filepath{info.rkt} Files}

The @filepath{info.rkt} files in the generated Racket packages
are built using @rel-link{guix/scripts/mk-info-rkt.rkt}.

@; ---------------------------------------------------------------------------------------------------
@; ---------------------------------------------------------------------------------------------------
@make-license-section[]
