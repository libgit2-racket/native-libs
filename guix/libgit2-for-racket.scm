(define-module (libgit2-for-racket)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages version-control))

;; We are NOT configuring with "-DDEPRECATE_HARD=ON"
;; for now, because we want to get things to build and
;; the test suite to pass.

(define-public %libgit2-version "1.3.0")

(define-public libgit2-origin
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/libgit2/libgit2")
          (commit (string-append "v" %libgit2-version))))
    (sha256
     (base32
      "0vgpb2175a5dhqiy1iwywwppahgqhi340i8bsvafjpvkw284vazd"))
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
       ((#:configure-flags _ '())
        (let* ((target (%current-target-system))
               (windows? (and target (string-contains target "mingw"))))
          `(list "-DREGEX_BACKEND=builtin" ;; maybe via Racket, one day?
                 "-DUSE_HTTP_PARSER=builtin"
                 "-DUSE_BUNDLED_ZLIB=ON"
                 "-DUSE_NTLMCLINT=OFF"
                 "-DUSE_SSH=OFF"
                 ,@(if windows?
                       (list (string-append "-DDLLTOOL=" target "-dlltool")
                             (string-append "-DCMAKE_RC_COMPILER="
                                            target "-windres")
                             ;; TODO use's racket's openssl
                             "-DCMAKE_C_FLAGS=-static-libgcc"
                             "-DCMAKE_CXX_FLAGS=-static-libgcc -static-libstdc++"
                             "-DCMAKE_EXE_LINKER_FLAGS=-static-libgcc -static-libstdc++"
                             "-DCMAKE_MODULE_LINKER_FLAGS=-static-libgcc -static-libstdc++")
                       (list "-DUSE_HTTPS=OpenSSL-Dynamic"))
                 ;; This part copied from the Guix package, because
                 ;; it's tricky to filter from the inherited arguments:
                 ,@(if (%current-target-system)
                       `((string-append
                          "-DPKG_CONFIG_EXECUTABLE="
                          (assoc-ref %build-inputs "pkg-config")
                          "/bin/" ,(%current-target-system) "-pkg-config"))
                       '()))))
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

