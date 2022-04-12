#lang scribble/base
@; SPDX-License-Identifier: (Apache-2.0 OR MIT)

@title{Native @tt{libgit2} packages for Racket: @tt{build-scripts}}

@(require "doc-utils.rkt"
          scribble/manual)

This repository packages the
@hyperlink["https://libgit2.org"]{libgit2} shared library
for the Racket package system.

The scripts contained in this branch (@tt{build-scripts})
are used to compile libgit2 for all supported platforms and
to pack the binaries into platform-specific Racket packages.
Each generated package is committed to its own ``orphan''
branch of this repository: the @tt{main} branch serves as a
navigational aide.

The build scripts are structured as a
@hyperlink["https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html"]{
 Nix flake} that generates
@hyperlink["https://guix.gnu.org"]{Guix} package
definitions.

This @rel-link{README.md} file itself is also generated
programmatically: see @secref{edit} for
instructions on editing and regenerating it.

@bold{Quick Start:} Try @exec{nix run} or @exec{make show}.
See @secref{run} for more.

@table-of-contents[]

@md-section{Prerequisites}

@itemlist[
 #:style 'ordered
 @item{Compiling libgit2 for Apple platforms currently requires
  an @tt{x86_64-macosx} build machine with the
  @hyperlink["https://nixos.org/manual/nix/stable/introduction.html"]{Nix}
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

@md-section[subsection]{Recommended Setup}

For maximum convenience, work on an @tt{x86_64-linux}
machine (a VM is fine) with both Nix and Guix installed. For
the Apple-specific compilation, set up an @tt{x86_64-macosx}
machine with Nix installed that you can access via SSH as a
Nix ``remote builder'': see the
@hyperlink["https://nixos.org/manual/nix/stable/advanced-topics/distributed-builds.html"]{Nix Manual}
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

 @itemlist[@item{Has the @exec{nix} command @envvar{PATH} of
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
           parallel, tests the built runners as desired (or maybe this
           is better left for the CI of the Racket @tt{libgit2} package
           itself); and}
          @item{Finally, exports the built packages in some
           useful way, perhaps even committing them to the appropriate
           branches.}]


@md-section[#:tag "run"]{Running the Build}

Write things here ...

@md-section[#:tag "edit"]{Changing Things}

Write things here ...

@make-license-section[]
