#lang scribble/base
@; SPDX-License-Identifier: (Apache-2.0 OR MIT)

@title{@tt[pkg-name]}

@(require "utils.rkt"
          racket/hash
          racket/cmdline
          racket/match
          (for-syntax racket/base)
          syntax/parse/define)

@(match-define
   (H-T arch+os
        lib-filename
        pkg-version
        breaking-change-label
        system-for-build
        platforms
        [#:_ self-source-info
         (H-T #:prefix self.
              [#:_ lastModifiedDate
               (app beautify-lastModifiedDate
                    self.lastModifiedDate)]
              narHash
              [#:? rev #f])]
        [#:_ nixpkgs-source+lock-info
         (H-T #:prefix nixpkgs.
              [#:_ lastModifiedDate
               (app beautify-lastModifiedDate
                    nixpkgs.lastModifiedDate)]
              narHash
              rev
              ;; .locked
              owner
              repo
              ;; .original
              ref)]
        [#:_ libgit2-info
         (H-T #:prefix libgit2.
              version
              sha256
              rev
              owner
              repo)])
   (let loop ([a (or (json-args) #hasheq())]
              [b default-json-args])
     (hash-union a b #:combine (λ (aa bb)
                                 (if (hash? aa)
                                     (loop aa bb)
                                     aa)))))

@(define canonical-repo
   "https://github.com/LiberalArtist/native-libgit2-pkgs")

@(define pkg-name
   (make-pkg-name breaking-change-label arch+os))
@(define (rel-link file)
   (hyperlink (++ "./" file) file))

This is the Racket package @tt[pkg-name], which contains the
@hyperlink["https://libgit2.org"]{libgit2} shared library built
for @tt[arch+os].


@section[#:style '(hidden-number)]{Provenance}

The contents of this package were generated using a
@hyperlink[
 "https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html"
 ]{Nix flake} with NAR hash @tt{@|self.narHash|}, last modified on
@|self.lastModifiedDate|. The flake is from Git commit
@(or self.rev @tt{DIRTY-@|self.lastModifiedDate|}): if you
are reading this file in a Git repository with the same
structure as @url[canonical-repo], that commit should be
part of the @tt{build-scripts} branch.

The flake was built on @tt[system-for-build] for @tt[arch+os].

The included libgit2 shared library is version
@|libgit2.version|, built from the source at
@url[(++ "https://github.com/"
         libgit2.owner "/" libgit2.repo
         "/tree/" libgit2.rev)].
The Nix sha256 hash of the source is @tt[libgit2.sha256].

@(define nixpkgs-base-url
   (++ "https://github.com/" nixpkgs.owner "/" nixpkgs.repo))
The build environment used the
@hyperlink[(++ nixpkgs-base-url "/tree/" nixpkgs.ref)]{@tt{@|nixpkgs.ref|}}
branch of @url[nixpkgs-base-url], which resolved to commit
@hyperlink[(++ nixpkgs-base-url "/commit/" nixpkgs.rev)]{@tt{@|nixpkgs.rev|}}
(last modified @|nixpkgs.lastModifiedDate|) with NAR hash
@tt{@|nixpkgs.narHash|}.


@section[#:style '(hidden-number)]{License}

The libgit2 shared library is under the license
@tt{(@hyperlink["https://spdx.org/licenses/GPL-2.0-only.html"]{@tt{GPL-2.0-only}} WITH
 @hyperlink["https://spdx.org/licenses/GCC-exception-2.0.html"]{@tt{GCC-exception-2.0}})}.
The exception grants ``unlimited permission to link the
compiled version of this library into combinations with
other programs, and to distribute those combinations without
any restriction coming from the use of'' libgit2: see the
@rel-link{COPYING} and @rel-link{README-libgit2.md} files
for further details. The @secref{Provenance} section of this
file explains how to get the corresponding source for the
compiled version of libgit2 distributed in this package.

(Note that @rel-link{AUTHORS} and similar files in this
repository are drawn from upstream libgit2, but the libgit2
maintainers are not responsible for this repository in any
way. (I am, however, grateful for advice some of them have
given.))

The build scripts and other miscellaneous files added as
part of the Racket packaging are distributed under the
@hyperlink["https://spdx.org/licenses/Apache-2.0.html"]{@tt{Apache-2.0}}
license and the @hyperlink["https://spdx.org/licenses/MIT.html"]{@tt{MIT}}
license, at your option (i.e@._ the same license as Racket).
By making a contribution, you are agreeing that your
contribution is licensed under the @tt{Apache-2.0} license
and the @tt{MIT} license.
