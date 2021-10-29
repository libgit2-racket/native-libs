#lang racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(require racket/runtime-path
         racket/match
         racket/function
         racket/file
         racket/promise
         racket/port
         (only-in racket/base
                  [string-append-immutable ++])
         "base/wrapped-common.rkt"
         "apple-nix-skel/invoke.rkt")

(provide ++
         guix-load-dir
         here
         current-workspace
         guix
         git
         target
         git-describe.txt/lazy-string
         read-only-permissions
         built-before-apple/
         built-before-apple/provenance/
         built-before-apple/provenance/git-describe.txt
         built-before-apple/shared-license-files/
         apple-nix-bundle/
         built-on-apple/
         (all-from-out "base/wrapped-common.rkt")
         (all-from-out "apple-nix-skel/invoke.rkt"))

(define-runtime-path guix-load-dir "guix/")
(define here (build-path guix-load-dir 'up))
(define default-workspace (build-path here "workspace"))
(define current-workspace
  (make-parameter default-workspace
                  path->complete-path))

(define guix (find-executable-path "guix"))
(define git (find-executable-path "git"))

(define-match-expander target
  (syntax-rules ()
    [(_ arch os triplet arch-os)
     (list arch os triplet arch-os)]))

(define (built-before-apple/)
  (build-path (current-workspace) "built-before-apple/"))
(define (built-before-apple/provenance/)
  (build-path (built-before-apple/) "provenance/"))
(define (built-before-apple/provenance/git-describe.txt)
  (build-path (built-before-apple/provenance/) "git-describe.txt"))
(define (built-before-apple/shared-license-files/)
  (build-path (built-before-apple/) "shared-license-files"))

(define (apple-nix-bundle/)
  (build-path (current-workspace) "apple-nix-bundle/"))
(define (built-on-apple/)
  (build-path (current-workspace) "built-on-apple/"))

(define read-only-permissions
  (bitwise-ior user-read-bit
               group-read-bit
               other-read-bit))

(define git-describe.txt/lazy-string
  (delay/sync
   (string->immutable-string
    (with-output-to-string
      (λ ()
        (parameterize ([current-directory here])
          (invoke git "describe" "--dirty" "--long" "--always")))))))
;TODO
;(define (check-git-describe who . extra-files
