(define-module (libgit2-for-racket)
  #:use-module (from-nix)
  #:use-module (apple)
  #:use-module (platforms)
  #:use-module (old-stable-libc)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #:use-module (guix utils)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages llvm)
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

(define-public (non-apple-libgit2->extracted lg2)
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

(define (extracted-for-non-apple-system guix-system)
  (cond
   ((target-mingw? guix-system)
    (with-parameters ((%current-target-system guix-system))
      (non-apple-libgit2->extracted libgit2)))
   (else
    (with-parameters ((%current-target-system
                       (and (not (equal? guix-system (%current-system)))
                            guix-system)))
      (non-apple-libgit2->extracted
       (package-with-old-stable-libc libgit2))))))

(define-public install-name-tool-shim
  (let ((src-pth "racket/src/mac/install_name_tool.rkt"))
    ;; racket-minimal is too minimal for `raco exe`
    (package
      (name "install-name-tool-shim")
      (version (string-append "0-rkt" (package-version racket)))
      (source (package-source racket-vm-cs))
      (native-inputs
       (list racket)) ;; ... cross ...
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~`((#$src-pth ,(string-append "lib/"
                                       (strip-store-file-name #$output)
                                       "/")))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'build
              (lambda* (#:key native-inputs inputs #:allow-other-keys)
                (with-directory-excursion #$output
                  (let ((rkt-src
                         (string-append "lib/"
                                        (strip-store-file-name #$output)
                                        "/install_name_tool.rkt"))
                        (raco
                         (search-input-file (or native-inputs inputs)
                                            "/bin/raco")))
                    (mkdir-p "bin")
                    (invoke raco "make" "--vv" rkt-src)
                    (invoke raco
                            "exe"
                            "--launcher"
                            "--vv"
                            "-o" "bin/install_name_tool"
                            rkt-src))))))))
      (home-page (string-append "https://github.com/racket/racket/blob/v"
                                (package-version racket)
                                "/"
                                src-pth))
      (synopsis "Limited shim for install_name_tool")
      (description "Darwin's @code{cctools} (similar to GNU @code{binutils}) includes a program called @code{install_name_tool}.  This package provides a very limited replacement, implemented in and used by Racket.")
      (license (list license:asl2.0 license:expat)))))

;; see derivation-log-file and log-file from (guix store)
(define-public platforms-extracted
  (append (map (match-lambda
                 ((racket-platform guix-system)
                  (list racket-platform
                        (extracted-for-non-apple-system guix-system))))
               non-apple-platforms)
          apple-platforms))

(define-public all-platform-names
  (map car platforms-extracted))

(define (make-branch-name suffix)
  (string-append breaking-change-label
                 (match breaking-change-label
                   ("" "")
                   (_ "-"))
                 suffix))
(define (make-pkg-name suffix)
  (string-append "libgit2-" (make-branch-name suffix)))


(define (make-libgit2-racket-package racket-platform extracted)
  (package
    (name (make-pkg-name racket-platform))
    (version pkg-version)
    (source extracted)
    (native-inputs
     (list patchelf llvm install-name-tool-shim racket))
    (build-system trivial-build-system)
    (arguments
     (list
      #:builder
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (mkdir-p #$output)
            (chdir #$output)
            (define dll-file?
              (file-name-predicate "\\.dll$"))
            (define dylib-file?
              (file-name-predicate "\\.dylib$"))
            (define srcdir
              #$(package-source this-package))
            (define lib-file-name
              (let ((lib-file? (lambda (file stat)
                                 (or (dll-file? file stat)
                                     (dylib-file? file stat)
                                     (elf-file? file)))))
                (match (with-directory-excursion srcdir
                         (find-files "." lib-file?))
                  ((found)
                   (basename found)))))
            (copy-file (string-append srcdir "/" lib-file-name)
                       lib-file-name)
            (unless (dll-file? lib-file-name #f)
              (make-file-writable lib-file-name)
              (cond
               ((dylib-file? lib-file-name #f)

                )
               (else
                (invoke (search-input-file %build-inputs
                                           "/bin/patchelf")
                        "--set-rpath"
                        "$ORIGIN"
                        lib-file-name))))
            #t))))
    (home-page "localhost")
    (synopsis "TODO")
    (description "TODO")
    (license
     (list license:asl2.0 license:expat license:gpl2))))

(filter-map (match-lambda
              ((_ #f)
               #f)
              ((racket-platform extracted)
               (make-libgit2-racket-package racket-platform extracted)))
            platforms-extracted)

(let ((cctools-version "973.0.1")
      (ld64-version "609")
      (revision "0")
      (commit "04663295d0425abfac90a42440a7ec02d7155fea"))
  (values #;package-with-c-toolchain
   (package
     (name "cctools")
     (version (git-version (string-append cctools-version
                                          "-ld64-"
                                          ld64-version)
                           revision
                           commit))
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/tpoechtrager/cctools-port")
              (commit commit)))
        (sha256
         (base32 "0vihfa8y64vvd3pxy8qh4mhcnzinxh9flpz9dvw4wch4zj2nnfjs"))
        (file-name (git-file-name name version))))
     (build-system (@ (guix build-system gnu) gnu-build-system))
     (native-inputs
      (list
       clang-toolchain))
     (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda args
               (chdir "cctools")))
           (add-after 'chdir 'find-linux-limits-h
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPATH"
                       (list->search-path-as-string
                        (cons #$(file-append
                                 (this-package-native-input "clang-toolchain")
                                 "/include")
                              (cond
                               ((getenv "CPATH")
                                => (cut search-path-as-string->list <>))
                               (else
                                '())))
                        ":")))))))
     (home-page "https://github.com/tpoechtrager/cctools-port")
     (synopsis "TODO")
     (description "TODO")
     (license license:apsl2))
 #;  `(("clang-toolchain" ,clang-toolchain))))
