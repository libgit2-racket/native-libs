#lang racket/base

(require ffi/unsafe
         rackunit)

(test-not-exn
 "test that libgit2 shared library can be found"
 (λ () 
   (ffi-lib (case (system-type)
              [(windows) "git2"]
              [else "libgit2"])
            '(#f))))
