(define-module (libgit2-for-racket)
  #:use-module (from-nix)
  #:use-module (apple)
  #:use-module (platforms)
  #:use-module (old-stable-libc)
  #:use-module (cctools)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
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
     (list patchelf llvm cctools racket))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules
      `((guix build gnu-build-system)
        (guix build utils)
        (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda args
              (mkdir "build")
              (chdir "build")))
          (delete 'bootstrap)
          (delete 'configure)
          (delete 'build)
          (delete 'check)
          (delete 'patch-shebangs)
          (delete 'strip)
          (delete 'make-dynamic-linker-cache)
          (delete 'install-license-files)
          (add-before 'install 'copy-shared-library
            (lambda* (#:key source native-inputs #:allow-other-keys)
              (define dll-file?
                (file-name-predicate "\\.dll$"))
              (define dylib-file?
                (file-name-predicate "\\.dylib$"))
              (define lib-file-name
                (let ((lib-file? (lambda (file stat)
                                   (or (dll-file? file stat)
                                       (dylib-file? file stat)
                                       (elf-file? file)))))
                  (match (with-directory-excursion source
                           (find-files "." lib-file?))
                    ((found)
                     (basename found)))))
              (copy-file (string-append source "/" lib-file-name)
                         lib-file-name)
              (setenv "LIB_FILE_NAME" lib-file-name)
              (setenv "LIB_FILE_TYPE"
                      (cond
                       ((dll-file? lib-file-name #f)
                        "dll")
                       ((dylib-file? lib-file-name #f)
                        "dylib")
                       (else
                        "elf")))))
          (add-after 'copy-shared-library 'patch-shared-library
            (lambda args
              (define lib-file-name
                (getenv "LIB_FILE_NAME"))
              (define lib-file-type
                (getenv "LIB_FILE_TYPE"))
              (unless (equal? "dll" lib-file-type)
                (make-file-writable lib-file-name)
                (match lib-file-type
                  ("elf"
                   (invoke "patchelf"
                           "--set-rpath"
                           "$ORIGIN"
                           lib-file-name))
                  ("dylib"
                   (patch-dylib #:lib-file-name lib-file-name))))))
          (replace 'install
            (lambda args
              (copy-recursively "." #$output))))
      #:validate-runpath? #f))
    (home-page "localhost")
    (synopsis "TODO")
    (description "TODO")
    (license
     (list license:asl2.0 license:expat license:gpl2))))

(define-public patch-dylib.scm
  (program-file
   "patch-dylib.scm"
   #~(begin
       (use-modules (ice-9 popen)
                    (ice-9 match)
                    (srfi srfi-1)
                    (guix build utils))
       (define lib-file-name
         (last (program-arguments)))
       ;; patch id
       (invoke "install_name_tool" "-id" lib-file-name lib-file-name)
       ;; patch libiconv
       (define dylibs-used
         (let* ((port (open-pipe* OPEN_READ
                                  "llvm-objdump"
                                  "--macho"
                                  "--dylibs-used"
                                  lib-file-name))
                (str (get-string-all port)))
           (close-pipe port)
           str))
       (define nix-libiconv
         ;; FIXME: Why doesn't guile like this regexp?
         (let* ((px
                 "(?<=\\s)/nix/store/\\S+/libiconv[\\d\\.]*\\.dylib(?=\\s)")
                (rkt-expr
                 `(write (regexp-match (pregexp ,px) ,dylibs-used)))
                (port (open-pipe* OPEN_READ
                                  "racket"
                                  "-e"
                                  (format #f "~s" rkt-expr)))
                (result (read port)))
           (close-pipe port)
           (match result
             ((str)
              str))))
       (invoke "install_name_tool"
               "-change"
               nix-libiconv
               "/usr/lib/libiconv.2.dylib"
               lib-file-name))))

(define-public platforms-packed
  (filter-map (match-lambda
                ((_ #f)
                 #f)
                ((racket-platform extracted)
                 (list racket-platform
                       (make-libgit2-racket-package racket-platform
                                                    extracted))))
              platforms-extracted))

(define-public omnibus
  (file-union
   "racket-native-libgit2-pkgs-bundle"
   platforms-packed))
omnibus

patch-dylib.scm
