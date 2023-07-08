(define-module (libgit2-for-racket)
  #:use-module (from-nix)
  #:use-module (extracted apple)
  #:use-module (extracted non-apple)
  #:use-module (platforms)
  #:use-module (cctools)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build json)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages racket))

;; see derivation-log-file and log-file from (guix store)
(define-public all-platforms-extracted
  (append non-apple-platforms-extracted
          apple-platforms-extracted))

(define-public all-platform-names
  (map car all-platforms-extracted))

(define local-scripts
  (local-file "scripts" #:recursive? #t))

(define-public racket-libgit2-omnibus
  (package
    (name "racket-libgit2-omnibus")
    (version pkg-version)
    (source #f)
    (native-inputs
     (cons libgit2-native-libs
           (map (cut apply make-platform-package <>)
                (filter cadr all-platforms-extracted))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~`(("." "."))
      #:imported-modules
      `((guix build union)
        (guix build json)
        ,@%copy-build-system-modules)
      #:modules
      `((guix build copy-build-system)
        (guix build utils)
        (guix build union)
        (guix build json)
        (srfi srfi-26)
        (ice-9 textual-ports)
        (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'patch-shebangs)
          (delete 'strip)
          (delete 'make-dynamic-linker-cache)
          (delete 'validate-runpath)
          (delete 'install-license-files)
          (replace 'unpack
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (union-build
               "build"
               (search-path-as-list '("rkt-pkgs")
                                    (map cdr (or native-inputs inputs))))
              (chdir "build"))))))
    (home-page
     "https://github.com/LiberalArtist/native-libgit2-pkgs/tree/build-scripts")
    (synopsis "All generated Racket libgit2 packages")
    (description "This Guix package combines all the generated Racket
libgit2 packages for distributing the shared library.")
    (license
     (list license:asl2.0 license:expat license:gpl2))))

(define (make-platform-package racket-platform extracted)
  (suffix->racket-pkg-libgit2
   racket-platform
   (package
     (inherit abstract-platform-package)
     (source extracted)
     (arguments
      (substitute-keyword-arguments
          (package-arguments abstract-platform-package)
        ((#:phases std-phases)
         #~(modify-phases #$std-phases
             (add-after 'patch-shared-library 'set-platform-sexpr-args
               (lambda args
                 (setenv "README_SCRBL" "platform-readme.scrbl")
                 (setenv "RKT_SEXPR_ARGS"
                         (format #f "~s"
                                 (vector #$racket-platform
                                         (getenv "LIB_FILE_NAME"))))))))))
     (synopsis "Platform-specific Racket libgit2 package")
     (description "This is a Racket package providing the libgit2
shared library build for a specific platform."))))

(define (make-branch-name suffix)
  (string-append breaking-change-label
                 (match breaking-change-label
                   ("" "")
                   (_ "-"))
                 suffix))
(define (make-pkg-name suffix)
  (string-append "libgit2-" (make-branch-name suffix)))
(define (make-home-page suffix)
  (string-append "https://pkgd.racket-lang.org/pkgn/package/"
                 (make-pkg-name suffix)))
     


(define racket-pkg-libgit2-abstract
  ;; parent of both abstract-platform-packacge and libgit2-native-libs
  (hidden-package
   (package
     (inherit racket-libgit2-omnibus)
     (name "racket-libgit2-abstract")
     (source #f)
     (inputs '())
     (propagated-inputs '())
     (native-inputs
      (list local-provenance-files
            racket))
     (arguments
      (strip-keyword-arguments
       '(#:install-plan) ;; wrapper will add it
       (substitute-keyword-arguments (package-arguments racket-libgit2-omnibus)
         ((#:phases std-phases)
          #~(modify-phases #$std-phases
              (replace 'unpack
                (lambda* (#:key native-inputs inputs #:allow-other-keys)
                  (union-build
                   "build"
                   (search-path-as-list '("rkt-pkg-skel")
                                        (map cdr (or native-inputs inputs)))
                   #:create-all-directories? #t)
                  (chdir "build")))
              (add-after 'unpack 'set-json-args
                (lambda* (#:key source #:allow-other-keys)
                  (define (get-built mode)
                    (cons mode (call-with-input-file
                                   (string-append "built-" mode ".txt")
                                 get-line)))
                  (define js
                    (with-directory-excursion
                        (string-append source "/provenance")
                      `(@ ("built"
                           @ ,@(map get-built
                                    '("by" "on")))
                          ("packed"
                           @ ("by" . "Guix")
                             ("on" . #$(%current-system))))))
                  (setenv "RKT_JSON_BUILT_PACKED_ARGS"
                          (call-with-output-string
                            (cut write-json js <>)))
                  (setenv "RKT_JSON_ARGS"
                          #$(call-with-output-string
                              (cut write-json
                                   (match all-from-nix-jsexpr
                                     (('@ . pairs)
                                      `(@ ("cfg-flags-windows"
                                           . ,cfg-flags-windows-jsexpr)
                                          ,@pairs)))
                                   <>)))
                  (setenv "RKT_NOT_A_DRILL" "1")))
              (add-after 'set-json-args 'mk-info-rkt
                (lambda args
                  (invoke "racket"
                          #$(file-append local-scripts "/mk-info-rkt.rkt")
                          "--out" "info.rkt")))
              (add-after 'mk-info-rkt 'readme-scrbl
                (lambda args
                  (invoke "scribble"
                          "--markdown"
                          "--link-section"
                          "--dest-name" "README.md"
                          (string-append #$local-scripts
                                         "/"
                                         (getenv "README_SCRBL")))))
              (add-after 'set-json-args 'write-self-rev
                (lambda args
                  (invoke
                   "racket"
                   "-e"
                   (format
                    #f "~s"
                    '(begin
                       (require (file #$(file-append local-scripts
                                                     "/args.rkt")))
                       (with-output-to-file "self.rev.txt"
                         (λ ()
                           (displayln
                            (or self.rev
                                (++ "DIRTY-" self.lastModifiedDate))))))))))
              (add-after 'set-json-args 'write-provenance-rktd
                ;; it could be provenance.json, but neither
                ;; Racket nor Guix has a pretty-printer
                (lambda args
                  (invoke
                   "racket"
                   "-e"
                   (format
                    #f "~s"
                    '(begin
                       (require (file #$(file-append local-scripts
                                                     "/args.rkt")))
                       (with-output-to-file "provenance.rktd"
                         (λ ()
                           (pretty-write
                            provenance-metadata-jsexpr)))))))))))))
     (home-page name)
     (synopsis name)
     (description name))))

(define* (suffix->racket-pkg-libgit2 suffix base)
  (package
    (inherit base)
    (name (make-pkg-name suffix))
    (arguments
     (cons*
      #:install-plan
      #~`(("." #$(string-append "rkt-pkgs/" suffix)))
      (package-arguments base)))
    (properties
     (filter (match-lambda
               ((k . v)
                (not (memq k '(hidden? rkt-pkg-suffix)))))
             (package-properties base)))
    (home-page (make-home-page suffix))))


(define-public libgit2-native-libs
  (suffix->racket-pkg-libgit2
   "native-libs"
   (package
     (inherit racket-pkg-libgit2-abstract)
     (source
      (file-union
       (string-append "racket-" (make-pkg-name "native-libs")
                      "-" pkg-version)
       `(("provenance"
          ,(local-file "aux-files/nix-provenance" #:recursive? #t)))))
     (arguments
      (substitute-keyword-arguments
          (package-arguments racket-pkg-libgit2-abstract)
        ((#:phases std-phases)
         #~(modify-phases #$std-phases
             (add-after 'unpack 'set-meta-sexpr-args
               (lambda args
                 (setenv "README_SCRBL" "meta-readme.scrbl")
                 (setenv "RKT_SEXPR_ARGS"
                         (format #f "~s" '#$all-platform-names))))))))
     (synopsis "Racket libgit2 platform meta-package")
     (description "This Racket package has platform-specific
dependencies on the libgit2 shared library metapackages."))))

(define abstract-platform-package
  (package
    (inherit racket-pkg-libgit2-abstract)
    (name "abstract-platform-package")
    (source #f) ;; concrete package must override
    (native-inputs
     (modify-inputs
      (package-native-inputs racket-pkg-libgit2-abstract)
      (prepend patchelf
               llvm
               cctools
               libgit2-shared-provenance-files)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments racket-pkg-libgit2-abstract)
       ((#:phases std-phases)
        #~(modify-phases #$std-phases
            (add-after 'unpack 'copy-shared-library
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
                     (invoke #$patch-dylib.scm
                             lib-file-name))))))))))))

(define-public libgit2-shared-provenance-files
  (package
    (inherit libgit2)
    (name "libgit2-shared-provenance-files")
    (outputs '("out"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs '())
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~`(("COPYING" "rkt-pkg-skel/")
          ("AUTHORS" "rkt-pkg-skel/")
          ("git.git-authors" "rkt-pkg-skel/")
          ("docs/changelog.md" "rkt-pkg-skel/")
          ("README.md" "rkt-pkg-skel/README-libgit2.md"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'install-license-files))))
    (synopsis "Files copied from the libgit2 origin for Racket packages")
    (description "This is a bundle of files like @code{COPYING} and
@code{git.git-authors} that have been extracted from the libgit2
origin and should be installed into Racket packages.")))


(define-public patch-dylib.scm
  (program-file
   "patch-dylib.scm"
   (with-imported-modules `((guix build utils))
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match)
                      (ice-9 popen)
                      (ice-9 textual-ports)
                      (srfi srfi-1))
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
                 lib-file-name)
         ;; Re-sign the dylibs with ad-hoc signatures, since we've
         ;; changed their contents (needed on aarch64).
         ;; See: https://github.com/bbusching/libgit2/issues/12
         ;; Implementation based on /racket/src/mac/codesign.rkt
         ;; in the Racket git repository.
         (invoke "racket"
                 "-e"
                 (format #f "~s"
                         `(begin
                            (require (file #$(file-append local-scripts
                                                          "/mach-o.rkt")))
                            (remove-signature ,lib-file-name)
                            (eprintf "signature removed: ~v\n"
                                     ,lib-file-name)
                            (add-ad-hoc-signature ,lib-file-name)
                            (eprintf "signature replaced: ~v\n"
                                     ,lib-file-name))))))))

(define-public local-provenance-files
  (file-union
   "racket-libgit2-build-scripts-provenance-files"
   `(("rkt-pkg-skel/LICENSE-Apache-2.0.txt"
      ,(local-file "aux-files/LICENSE-Apache-2.0.txt"))
     ("rkt-pkg-skel/LICENSE-MIT.txt"
      ,(local-file "aux-files/LICENSE-MIT.txt"))
     ("rkt-pkg-skel/.gitignore"
      ,(local-file "aux-files/gitignore-skel"))
     ("rkt-pkg-skel/flake.lock"
      ,(local-file "aux-files/flake.lock"))
     ("rkt-pkg-skel/channels.txt" ;; placate `raco setup`
      ,(local-file "../channels.scm")))))

