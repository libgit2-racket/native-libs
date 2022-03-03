#! /Users/philip/.nix-profile/bin/nix-shell
#! nix-shell -i racket -p nix llvm racket-minimal
#! nix-shell -I ./nixpkgs
#! nix-shell --pure
;; HACK!!! need to put nix-shell into PATH for interactive non-login shells
;; see nix ping-store
;; https://nixos.org/manual/nix/stable/#use-as-a-interpreter
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)
#lang racket/base

(require racket/runtime-path
         "invoke.rkt"
         racket/match
         racket/file
         racket/port
         racket/cmdline)

(module+ main
  (match (system-type 'os*)
    [(or 'macosx 'darwin)
     (copy-git-describe.txt)
     (for-each build-one-arch
               (file->value platforms.rktd))]
    [os*
     (error 'build-all.rkt "not running on a Mac\n  os*: ~e" os*)]))

(define-runtime-path here ".")
(define (here/ . args)
  (apply build-path here args))

(define-values [workspace platforms.rktd]
  (let ([workspace (here/ 'up)]
        [platforms (here/ "platforms.rktd")])
    (command-line
     #:once-each
     [("--workspace") dir "use <dir> instead of this file's parent"
                     (set! workspace (path->complete-path dir))]
     [("--platforms") platforms.rktd "use rather than this file's sibling"
                     (set! platforms platforms.rktd)]
     #:args ()
     (values workspace platforms))))

(define built-dir
  (build-path workspace "built-on-apple"))
(define tmp-store-root
  (build-path workspace "tmp-store-root"))

(define nix-build
  (find-executable-path "nix-build"))
(define objdump
  (find-executable-path "llvm-objdump"))
(define install_name_tool
  ;; llvm seems to pull this in via cctools-binutils-darwin
  (find-executable-path "install_name_tool"))
(define codesign
  (string->path "/usr/bin/codesign"))

(define build-one-arch
  (match-lambda
    [(list pkgsCrossAttr built-file-path arch-os filename)
     (define dest-pth
       (build-path built-dir "pkgs" arch-os filename))
     (make-parent-directory* dest-pth)
     (invoke nix-build
             "--out-link" tmp-store-root
             (and pkgsCrossAttr
                  (list "--argstr" "pkgsCrossAttr" pkgsCrossAttr))
             (here/ "build-one.nix"))
     (copy-file (build-path tmp-store-root built-file-path)
                dest-pth)
     (delete-file tmp-store-root)
     (define old-mode (file-or-directory-permissions dest-pth 'bits))
     (file-or-directory-permissions dest-pth (bitwise-ior user-write-bit old-mode))
     (invoke install_name_tool "-id" filename dest-pth)
     (define dylibs-used
       (with-output-to-string
         (λ ()
           (invoke objdump "-macho" "--dylibs-used" dest-pth))))
     (invoke install_name_tool
             "-change"
             (match dylibs-used
               [(pregexp #px"(?<=\\s)/nix/store/\\S+/libiconv[\\d\\.]*\\.dylib(?=\\s)"
                         (list nix-lib))
                nix-lib])
             "/usr/lib/libiconv.2.dylib"
             dest-pth)
     (file-or-directory-permissions dest-pth old-mode)]))

(define (copy-git-describe.txt)
  (define txt "git-describe.txt")
  (define from-pth (here/ 'up txt))
  (when (file-exists? from-pth)
    (make-directory* built-dir)
    (copy-file from-pth
               (build-path built-dir txt))))


    
