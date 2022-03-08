#lang scribble/base
@; SPDX-License-Identifier: (Apache-2.0 OR MIT)

@title{@tt[pkg-name]}

@(require "doc-utils.rkt"
          racket/match
          (for-syntax racket/base)
          syntax/parse/define)

@(match-define (H-T arch+os lib-filename) json-args/defaults)

@(define pkg-name
   (make-pkg-name* arch+os))
@(define (rel-link file)
   (hyperlink (++ "./" file) file))

This is the Racket package @tt[pkg-name], which contains the
@hyperlink["https://libgit2.org"]{libgit2} shared library built
for @tt[arch+os].


@make-provenance-section[#:arch+os arch+os]


@md-section{License}

The libgit2 shared library is under the license
@tt{(@hyperlink["https://spdx.org/licenses/GPL-2.0-only.html"]{@tt{GPL-2.0-only}} WITH
 @hyperlink["https://spdx.org/licenses/GCC-exception-2.0.html"]{@tt{GCC-exception-2.0}})}.
The exception grants ``unlimited permission to link the
compiled version of'' libgit2 ``into combinations with
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
license or the @hyperlink["https://spdx.org/licenses/MIT.html"]{@tt{MIT}}
license, at your option (i.e@._ the same license as Racket).
By making a contribution, you are agreeing that your
contribution is licensed under the @tt{Apache-2.0} license
and the @tt{MIT} license.
