#lang at-exp racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(provide (all-from-out "utils.rkt")
         canonical-repo
         make-pkg-name*
         make-branch-name*
         md-section
         rel-link
         make-provenance-section
         make-license-section)

(require "utils.rkt"
         "args.rkt"
         racket/match
         scribble/base)

(define canonical-repo
  "https://github.com/LiberalArtist/native-libgit2-pkgs")

(define (make-pkg-name* suffix)
  (make-pkg-name breaking-change-label suffix))
(define (make-branch-name* suffix)
  (make-branch-name breaking-change-label suffix))

(define (md-section . args)
  (apply section #:style '(hidden-number) args))

(define (rel-link file)
  (hyperlink (++ "./" file) file))

(define (make-provenance-section #:arch+os [arch+os #f])
  (define nixpkgs-base-url
    (++ "https://github.com/" nixpkgs.owner "/" nixpkgs.repo))
  @list{
 @md-section{Provenance}

 @(if arch+os
      @list{The contents of this package}
      @list{These contents of these packages, including this meta-package,})
 were generated using a
 @hyperlink[
 "https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html"
 ]{Nix flake} with NAR hash @tt{@|self.narHash|}, last modified on
 @|self.lastModifiedDate|. The flake is from Git commit
 @(if self.rev
      (hyperlink (++ canonical-repo "/commit/" self.rev)
                 @tt[self.rev])
      @tt{DIRTY-@|self.lastModifiedDate|}): if you
 are reading this file in a Git repository with the same
 structure as @url[canonical-repo], that commit should be
 part of the @hyperlink[
 (++ canonical-repo "/tree/build-scripts")]{@tt{build-scripts}} branch.

 The flake was built on @tt[system-for-build]
 @(if arch+os
      @list{for @tt[arch+os].}
      @list{to generate this package, @make-pkg-name*{native-libs}.})

 The included libgit2 shared library is version
 @|libgit2.version|, built from the source at
 @url[(++ "https://github.com/"
          libgit2.owner "/" libgit2.repo
          "/tree/" libgit2.rev)].
 The Nix sha256 hash of the source is @tt[libgit2.sha256].

 The build environment used the
 @hyperlink[(++ nixpkgs-base-url "/tree/" nixpkgs.ref)]{@tt{@|nixpkgs.ref|}}
 branch of @url[nixpkgs-base-url], which resolved to commit
 @hyperlink[(++ nixpkgs-base-url "/commit/" nixpkgs.rev)]{@tt{@|nixpkgs.rev|}}
 (last modified @|nixpkgs.lastModifiedDate|) with NAR hash
 @tt{@|nixpkgs.narHash|}.
 })


(define (make-license-section #:see [see-content #f])
  @list{
 @md-section{License}

 The libgit2 shared library is under the license
 @tt{(@hyperlink["https://spdx.org/licenses/GPL-2.0-only.html"]{@tt{GPL-2.0-only}} WITH
  @hyperlink["https://spdx.org/licenses/GCC-exception-2.0.html"]{@tt{GCC-exception-2.0}})}.
 The exception grants ``unlimited permission to link the
 compiled version of'' libgit2 ``into combinations with
 other programs, and to distribute those combinations without
 any restriction coming from the use of'' libgit2: see
 @(or see-content
      @list{the platform-specific packages for further details.})

 The build scripts and other miscellaneous files added as
 part of the Racket packaging are distributed under the
 @hyperlink["https://spdx.org/licenses/Apache-2.0.html"]{@tt{Apache-2.0}}
 license or the @hyperlink["https://spdx.org/licenses/MIT.html"]{@tt{MIT}}
 license, at your option (i.e@._ the same license as Racket).
 By making a contribution, you are agreeing that your
 contribution is licensed under the @tt{Apache-2.0} license
 and the @tt{MIT} license.
 })
