(define-module (extracted non-apple)
  #:use-module (from-nix)
  #:use-module (old-stable-libc)
  #:use-module (platforms)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module ((gnu packages version-control)
                #:select ((libgit2 . super:libgit2)))
  #:export (non-apple-platforms-extracted
            libgit2))

;; The purpose of this module is to produce "extracted"
;; G-expressions for non-Apple platforms: that's the same
;; thing we get in the module (extracted apple), which is
;; generated by Nix.

(define (make-extracted-non-apple racket-platform guix-system)
  (cond
   ((target-mingw? guix-system)
    (with-parameters ((%current-target-system guix-system))
      (built->extracted-gexp racket-platform libgit2)))
   (else
    (with-parameters ((%current-target-system
                       (and (not (equal? guix-system (%current-system)))
                            guix-system)))
      (built->extracted-gexp racket-platform
                             (package-with-old-stable-libc libgit2))))))

(define non-apple-platforms-extracted
  (map (match-lambda
         ((racket-platform guix-system)
          (list racket-platform
                (make-extracted-non-apple racket-platform guix-system))))
       non-apple-platforms))

(define-public libgit2
  (package
    (inherit super:libgit2)
    (name "libgit2")
    (version libgit2-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url libgit2-url)
                    (commit libgit2-rev)))
              (sha256 (base32 libgit2-sha256))
              (file-name (git-file-name name version))))
    (inputs '())
    (propagated-inputs '())
    (native-inputs
     (list python))
    (arguments
     (substitute-keyword-arguments (package-arguments super:libgit2)
       ((#:tests? _ #t)
        ;; TODO: need to work around OpenSSL-Dynamic
        #f)
       ((#:phases std-phases #~%standard-phases)
        #~(modify-phases #$std-phases
            (add-before 'check 'add-clar-alias
              (lambda args
                ;; workaround until the Guix definition moves to 1.4 series
                (when (file-exists? "libgit2_tests")
                  ;; it would have .exe for mingw, which we can't run anyway
                  (copy-file "libgit2_tests" "libgit2_clar"))))))
       ((#:configure-flags _ ''())
        #~(append
           #$(let ((sys (or (%current-target-system)
                            (%current-system))))
               (if (target-mingw? sys)
                   (make-cfg-flags-mingw sys)
                   #~`(#$@cfg-flags-unix)))
           `(#$@cfg-flags-common)))))))

(define-public (built->extracted-gexp racket-platform lg2)
  (computed-file
   (string-append "extracted-" racket-platform)
   (with-imported-modules `((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (mkdir-p #$output)
         (define windows?
           #$(and (target-mingw?) #t))
         (define lib-file-name
           (format #f
                   (if windows? "libgit2-~a.dll" "libgit2.so.~a")
                   #$so-version))
         (define built-lib-path
           (if windows?
               "bin/libgit2.dll"
               (string-append "lib/" lib-file-name)))
         (copy-file (string-append #$lg2 "/" built-lib-path)
                    (string-append #$output "/" lib-file-name))
         (with-directory-excursion #$output
           (mkdir-p "provenance")
           (with-directory-excursion "provenance"
             (for-each (lambda (file content)
                         (with-output-to-file file
                           (lambda ()
                             (format #t "~a\n" content))))
                       `("built-by.txt" "built-on.txt")
                       `("Guix"         #$(%current-system)))))))))
