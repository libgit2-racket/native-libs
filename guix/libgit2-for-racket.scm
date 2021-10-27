(define-module (libgit2-for-racket)
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages elf)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (libgit2-for-racket common))

;; We are NOT configuring with "-DDEPRECATE_HARD=ON"
;; for now, because we want to get things to build and
;; the test suite to pass.

(define-public (target->system-flags target)
  (cond
   ((target-windows? target)
    `(,(string-append "-DDLLTOOL=" target "-dlltool")
      ,(string-append "-DCMAKE_RC_COMPILER=" target "-windres")
      ;; TODO use's racket's openssl
      "-DCMAKE_C_FLAGS=-static-libgcc"
      "-DCMAKE_CXX_FLAGS=-static-libgcc -static-libstdc++"
      "-DCMAKE_EXE_LINKER_FLAGS=-static-libgcc -static-libstdc++"
      "-DCMAKE_MODULE_LINKER_FLAGS=-static-libgcc -static-libstdc++"))
   (else
    `("-DUSE_HTTPS=OpenSSL-Dynamic"))))

(define-public (target-windows? target)
  (and target (string-contains target "mingw")))

(define-public libgit2-origin
  (origin
    (method git-fetch)
    (uri (git-reference
          (url %libgit2-origin-git-url)
          (commit %libgit2-origin-commit)))
    (sha256 (base32 %libgit2-checksum))
    (patches
     (origin-patches (package-source libgit2)))
    (file-name (git-file-name "libgit2" %libgit2-version))))

(define-public libgit2-shared-license-data
  (package
    (name "libgit2-shared-license-data")
    (version %libgit2-version)
    (source libgit2-origin)
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("COPYING" "COPYING")
         ("AUTHORS" "AUTHORS")
         ("git.git-authors" "git.git-authors")
         ("docs/changelog.md" "changelog.md")
         ("SECURITY.md" "SECURITY.md"))
       #:phases
       (modify-phases %standard-phases
         (delete 'install-license-files)
         (delete 'compress-documentation))))
    (home-page (package-home-page libgit2))
    (synopsis "Files copied from the libgit2 repository")
    (description
     "This package contains license files and such that should be
copied into native @code{libgit2} Racket packages.")
    (license (package-license libgit2))))

(define-public libgit2-for-racket
  (package
    (inherit libgit2)
    (version %libgit2-version)
    (source libgit2-origin)
    (inputs `())
    (propagated-inputs `())
    (arguments
     (substitute-keyword-arguments (package-arguments libgit2)
       ((#:configure-flags std-cfg-flags '())
        `(append
          ',(target->system-flags (%current-target-system))
          ',%common-configure-flags
          (let ((flags ,std-cfg-flags))
            (filter (lambda (s)
                      (string-prefix? "-DPKG_CONFIG_EXECUTABLE=" s))
                    flags))))
       ((#:tests? _ #f)
        ;; error trying to write
        #f)
       #;
       ((#:phases usual-phases)
       `(modify-phases ,usual-phases
       (add-before 'configure 'mut-src
       (lambda _
       (copy-recursively "." "../mut-srcdir")
       (chdir "../mut-srcdir")))
       #;
       (add-before 'test 'set-clar-tmp
       (lambda _
       (mkdir-p "my-clar-tmp")
       (setenv "CLAR_TMP" "my-clar-tmp")))))))))


(define-public extract-lib/g
  (with-imported-modules `((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$output)
        (chdir #$output)
        (copy-file
         #$(let-system
            (system target)
            (file-append
             libgit2-for-racket
             (if (target-windows? target)
                 "/bin/libgit2.dll"
                 (string-append "/lib/"
                                (os->lib-filename 'linux)))))
         #$(let-system
            (system target)
            (os->lib-filename 
             (if (target-windows? target)
                 'win32
                 'linux)))))))
(define-public patch-rpath/g
  (with-imported-modules `((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (invoke #+(file-append patchelf "/bin/patchelf")
                "--set-rpath" "$ORIGIN"
                #$(os->lib-filename 'linux)))))

(define-public built-here-dirs
  (file-union
   "build"
   (filter-map
    (match-lambda
      ((arch os target)
       (if (apple-os? os)
           #f
           (let ((name (format #f "~a-~a" arch os)))
             (list name
                   (with-parameters
                       ((%current-target-system
                         (or target (%current-target-system))))
                     (computed-file name extracted-lib)))))))
    %all-platforms)))
