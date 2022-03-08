#lang at-exp racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(provide (all-from-out "utils.rkt")
         canonical-repo
         make-pkg-name*
         make-branch-name*
         md-section
         make-provenance-section
         json-args/defaults
         pkg-version
         breaking-change-label
         system-for-build
         platforms
         self.lastModifiedDate
         self.narHash
         self.rev
         nixpkgs.lastModifiedDate
         nixpkgs.narHash
         nixpkgs.rev
         nixpkgs.owner
         nixpkgs.repo
         nixpkgs.ref
         libgit2.version
         libgit2.sha256
         libgit2.rev
         libgit2.owner
         libgit2.repo)

(require "utils.rkt"
         racket/match
         scribble/base)

(define require-args-env-var-name
  "RKT_ENFORCE_ARGS")

(define json-args/defaults
  (cond
    [(json-args)]
    [(not (getenv require-args-env-var-name))
     default-json-args]
    [else
     ;; ... error ...
     #f]))  

(match-define
  (H-T #;arch+os
       #;lib-filename
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
  json-args/defaults)

(define canonical-repo
  "https://github.com/LiberalArtist/native-libgit2-pkgs")

(define (make-pkg-name* suffix)
  (make-pkg-name breaking-change-label suffix))
(define (make-branch-name* suffix)
  (make-branch-name breaking-change-label suffix))

(define (md-section . args)
  (apply section #:style '(hidden-number) args))

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
 @(or self.rev @tt{DIRTY-@|self.lastModifiedDate|}): if you
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

        
