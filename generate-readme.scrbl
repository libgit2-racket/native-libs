#lang scribble/base
@; SPDX-License-Identifier: (Apache-2.0 OR MIT)

@(require racket/cmdline
          (for-syntax racket/base)
          syntax/parse/define
          json)
@(define args
   (command-line
    #:args ([json "{}"])
    (string->jsexpr json)))
@(define-syntax-parse-rule (define-arg arg:id (~optional default:expr))
   (define arg (hash-ref args 'arg (~? default))))
@(define-arg arch+os "UNKNOWN_ARCH+OS")

@title{libgit2-@|arch+os|}

