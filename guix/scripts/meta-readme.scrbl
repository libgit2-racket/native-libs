#lang scribble/base
@; SPDX-License-Identifier: (Apache-2.0 OR MIT)

@title{@tt[pkg-name]}

@(require "doc-utils.rkt"
          "args.rkt"
          racket/string
          racket/match)

@(match-sexpr-args (and (list (? non-empty-string?) ...)
                        (app (λ (lst) (sort lst arch+os<?))
                             platforms))
                   '("ppc32-solaris" "i386-linux" "x86_64-freebsd" "aarch64-freebsd"))
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


@make-license-section[]
