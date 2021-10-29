#lang racket
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(require "base.rkt")

(provide init)

(define (init)
  (parameterize ([current-directory here])
    (delete-directory/files (current-workspace)
                            #:must-exist? #f)
    (make-directory (current-workspace))
    (build-all-non-apple-targets)
    (build-provenance+license-data)
    (build-apple-nix-bundle)))

(define guix-build
  (curry invoke guix "build" "-L" guix-load-dir "--keep-failed"))

(define (make-git-describe.txt pth)
  (call-with-output-file* pth
    #:permissions read-only-permissions
    (λ (out)
      (write-string (force git-describe.txt/lazy-string) out)))
  (void))


(define (build-apple-nix-bundle)
  (make-directory* (apple-nix-bundle/))
  (make-git-describe.txt
   ;; This goes outside of the Guix-created apple-nix-src directory
   ;; because there may be many commits that would produce identical
   ;; sources: no need to trigger unnecessary rebuilds.
   (build-path (apple-nix-bundle/) "git-describe.txt"))
  (guix-build "-e" "(@ (libgit2-for-racket apple-via-nix) apple-nix-src)"
              "-r" (build-path (apple-nix-bundle/) "apple-nix-src")))


(define (build-provenance+license-data)
  (make-directory* (built-before-apple/provenance/))
  (guix-build "libgit2-shared-license-files"
              "-r" (built-before-apple/shared-license-files/))
  (make-git-describe.txt (built-before-apple/provenance/git-describe.txt))
  (with-output-to-file (build-path (built-before-apple/provenance/)
                                   "guix-channels.scm")
    #:permissions read-only-permissions
    (λ ()
      (printf ";; -*- mode: scheme; -*-\n")
      (invoke guix "describe" "-f" "channels"))))


(define (build-all-non-apple-targets)
  (for-each (match-lambda
              [(target arch os triplet arch-os)
               #:when (apple-os? os)
               (void)]
              [a-target
               (build-non-apple-target a-target)])
            %all-platforms))

(define (build-non-apple-target a-target)
  (match-define (target arch os triplet arch-os) a-target)
  (define dest-dir (build-path (built-before-apple/) "pkgs" arch-os))
  (delete-directory/files dest-dir #:must-exist? #f)
  (make-directory* dest-dir)
  (define filename (os->lib-filename os))
  (define dest-pth (build-path dest-dir filename))
  ;; build & copy:
  (let ([tmp-gc-root-dir
         (make-temporary-file "tmp-gc-root-dir~a" 'directory)])
    (dynamic-wind
     void
     (λ ()
       (define store-dir
         (second
          (string-split
           (with-output-to-string
             (λ ()
               (parameterize ([current-directory here])
                 (guix-build "-e"
                             "(@ (libgit2-for-racket) libgit2-for-racket)"
                             "-r" (build-path tmp-gc-root-dir "built") ;; built-0 and -1
                             (and triplet (++ "--target=" triplet)))))))))
       (copy-file (build-path store-dir (os->built-lib-path os))
                  dest-pth))
     (λ ()
       (delete-directory/files tmp-gc-root-dir))))
  ;; patch:
  (when (eq? 'linux os)
    (define mode (file-or-directory-permissions dest-pth 'bits))
    (file-or-directory-permissions dest-pth (bitwise-ior mode user-write-bit))
    (invoke guix "shell" "--pure" "--container" "patchelf"
            "--"
            "patchelf" "--set-rpath" "$ORIGIN" dest-pth)
    (file-or-directory-permissions dest-pth mode)))
