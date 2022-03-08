#lang scribble/base
@; SPDX-License-Identifier: (Apache-2.0 OR MIT)

@title{@tt[pkg-name]}

@(require "doc-utils.rkt"
          racket/match)

@(define pkg-name
   (make-pkg-name* "native-libs"))

This is the Racket package @tt[pkg-name], a meta-package
distributing the native
@hyperlink["https://libgit2.org"]{libgit2} shared library
via platform-specific dependencies in the Racket package
system.

Currently, the pre-built library is provided for the
following platforms:
@(apply itemlist
        (for/list ([arch+os (in-list (sort platforms arch+os<?))])
          @item{@tt[arch+os] (package: @hyperlink[
 (++ canonical-repo "/tree/" (make-branch-name* arch+os))
 @tt[(make-pkg-name* arch+os)]])}))

If you are reading this file in a Git repository with the
same structure as @url[canonical-repo], you can find the
platform-specific packages on sibling branches of this one.
The @hyperlink[canonical-repo]{@tt{main}} branch may help you
find the relevant branches and commits.


@make-provenance-section[]


@md-section{License}

The libgit2 shared library is under the license
@tt{(@hyperlink["https://spdx.org/licenses/GPL-2.0-only.html"]{@tt{GPL-2.0-only}} WITH
 @hyperlink["https://spdx.org/licenses/GCC-exception-2.0.html"]{@tt{GCC-exception-2.0}})}.
The exception grants ``unlimited permission to link the
compiled version of'' libgit2 ``into combinations with
other programs, and to distribute those combinations without
any restriction coming from the use of'' libgit2: see the
platform-specific packages for further details.

The build scripts and other miscellaneous files added as
part of the Racket packaging are distributed under the
@hyperlink["https://spdx.org/licenses/Apache-2.0.html"]{@tt{Apache-2.0}}
license or the @hyperlink["https://spdx.org/licenses/MIT.html"]{@tt{MIT}}
license, at your option (i.e@._ the same license as Racket).
By making a contribution, you are agreeing that your
contribution is licensed under the @tt{Apache-2.0} license
and the @tt{MIT} license.
