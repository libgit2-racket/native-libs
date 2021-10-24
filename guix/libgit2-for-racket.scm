(define-module (libgit2-for-racket)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages version-control)
  #:use-module (racket-libgit2-build-constants))

;; We are NOT configuring with "-DDEPRECATE_HARD=ON"
;; for now, because we want to get things to build and
;; the test suite to pass.

(define-public pinned-nixpkgs
  (origin
    (method url-fetch)
    (uri %nixpkgs-url)
    (sha256 (base32 %nixpkgs-checksum))))

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


(define-public (target->system-flags target)
  (cond
   ((and target (string-contains target "mingw"))
    `(,(string-append "-DDLLTOOL=" target "-dlltool")
      ,(string-append "-DCMAKE_RC_COMPILER=" target "-windres")
      ;; TODO use's racket's openssl
      "-DCMAKE_C_FLAGS=-static-libgcc"
      "-DCMAKE_CXX_FLAGS=-static-libgcc -static-libstdc++"
      "-DCMAKE_EXE_LINKER_FLAGS=-static-libgcc -static-libstdc++"
      "-DCMAKE_MODULE_LINKER_FLAGS=-static-libgcc -static-libstdc++"))
   (else
    `("-DUSE_HTTPS=OpenSSL-Dynamic"))))

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

