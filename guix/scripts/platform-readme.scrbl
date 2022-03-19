#lang scribble/base
@; SPDX-License-Identifier: (Apache-2.0 OR MIT)

@title{@tt[pkg-name]}

@(require "doc-utils.rkt"
          "args.rkt"
          racket/match
          (for-syntax racket/base)
          syntax/parse/define)

@(match-sexpr-args (vector arch+os lib-filename)
                   #("ppc32-solaris" "my-lib.so.x.y.z"))

@(define pkg-name
   (make-pkg-name* arch+os))

This is the Racket package @tt[pkg-name], which contains the
@hyperlink["https://libgit2.org"]{libgit2} shared library built
for @tt[arch+os].


@make-license-section[
 #:see @list{
  the
  @rel-link{COPYING} and @rel-link{README-libgit2.md} files
  for further details.

  The @secref{Provenance} section of this file explains how
  to get the corresponding source for the compiled version of
  libgit2 distributed in this package.

  (Note that @rel-link{AUTHORS} and similar files in this
  repository are drawn from upstream libgit2, but the libgit2
  maintainers are not responsible for this repository in any
  way. (I am, however, grateful for advice some of them have
  given.))
  }]


@make-provenance-section[#:arch+os arch+os]
