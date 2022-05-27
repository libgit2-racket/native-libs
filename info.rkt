#lang info
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(define pkg-name "libgit2-i386-win32")
(define collection "libgit2")

(define version "0.1")
(define pkg-desc "native libraries for \"libgit2\" on \"i386-win32\"")
(define pkg-authors '(philip))

(define install-platform "win32\\i386")
(define copy-foreign-libs '("libgit2-1.4.dll"))

(define license
  '((GPL-2.0-only WITH GCC-exception-2.0)
    AND
    (Apache-2.0 OR MIT)))

(define deps '("base"))
