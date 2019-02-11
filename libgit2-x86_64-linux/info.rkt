#lang info

(define collection "libgit2")
(define pkg-desc "native libraries for \"libgit2\" on \"x86_64-linux\"")
(define version "0.0")
(define pkg-authors '(philip))

(define install-platform #rx"^x86_64-linux(?:-natipkg)?$")
(define copy-foreign-libs '("libgit2.so"))

(define deps '("base"))

