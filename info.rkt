#lang info
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(define pkg-name "libgit2-x86_64-macosx")
(define collection "libgit2")

(define version "0.1")
(define pkg-desc "native libraries for \"libgit2\" on \"x86_64-macosx\"")
(define pkg-authors '(philip))

(define install-platform "x86_64-macosx")
(define copy-foreign-libs '("libgit2.1.4.dylib"))

(define license
  '((GPL-2.0-only WITH GCC-exception-2.0)
    AND
    (Apache-2.0 OR MIT)))

(define deps '("base"))
