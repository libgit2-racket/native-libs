(define-module (libgit2-for-racket)
  #:use-module (from-nix)
  #:use-module (platforms)
  #:use-module (old-stable-libc)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages racket)
  #:use-module ((gnu packages version-control)
                #:select ((libgit2 . super:libgit2))))

(define-public libgit2
  (package
    (inherit super:libgit2)
    (name "libgit2")
    (version libgit2-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url libgit2-url)
                    (commit libgit2-commit)))
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

(define-public (make-cfg-flags-mingw sys)
  #~`("-DCMAKE_C_FLAGS=-static-libgcc"
      "-DCMAKE_CXX_FLAGS=-static-libgcc"
      "-DCMAKE_EXE_LINKER_FLAGS=-static-libgcc"
      "-DCMAKE_MODULE_LINKER_FLAGS=-static-libgcc"
      #$(string-append "-DCMAKE_RC_COMPILER=" sys "-windres")
      #$(string-append "-DDLLTOOL=" sys "-dlltool")))

(define-public (extract-non-apple lg2)
  (computed-file
   "extracted-non-apple"
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
         (with-output-to-file (string-append #$output "/built-on.txt")
           (lambda ()
             (format #t "~a\n" #$(%current-system))))
         (copy-file (string-append #$lg2 "/" built-lib-path)
                    (string-append #$output "/" lib-file-name))))))

#;
(define-public install_name_tool.rkt
  (file-append (package-source racket-vm-cs)
               "racket/src/mac/install_name_tool.rkt"))

;; see derivation-log-file and log-file from (guix store)
(define-public extracted
  (map (match-lambda
         ((racket-platform guix-system)
          (cond
           ((target-mingw? guix-system)
            (with-parameters ((%current-target-system guix-system))
              (extract-non-apple libgit2)))
           (else
            (with-parameters ((%current-target-system
                               (and (not (equal? guix-system (%current-system)))
                                    guix-system)))
              (extract-non-apple
               (package-with-old-stable-libc libgit2)))))))
       non-apple-platforms))
extracted
