#lang info

(define pkg-name "libgit2-native-libs")
(define collection 'multi)

(define version "0.0")
(define pkg-authors
  '(philip))
(define pkg-desc "native libraries for \"libgit2\" (meta-package)")

(define deps
  `("base"
    ["libgit2-x86_64-linux" #:platform #rx"^x86_64-linux(?:-natipkg)?$"
                            #:version ,version]
    ["libgit2-x86_64-macosx" #:platform "x86_64-macosx"
                             #:version ,version]
    ;; coming soon ...
    #;
    ["libgit2-aarch64-macosx" #:platform "aarch64-macosx"
                              #:version ,version]
    #;
    ["libgit2-i386-win32" #:platform "win32\\i386"
                          #:version ,version]
    #;
    ["libgit2-x86_64-win32" #:platform "win32\\x86_64"
                            #:version ,version]
    ))

(define license
  '(Apache-2.0 OR MIT))
