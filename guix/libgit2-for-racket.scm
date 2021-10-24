(define-module (libgit2-for-racket)
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages version-control)
  #:use-module (ice-9 match)
  #:use-module (racket-libgit2-build-constants))

;; We are NOT configuring with "-DDEPRECATE_HARD=ON"
;; for now, because we want to get things to build and
;; the test suite to pass.

(define-public pinned-nixpkgs
  (origin
    (method url-fetch)
    (uri %nixpkgs-url)
    (sha256 (base32 %nixpkgs-checksum))))

(define-public repo-root-dir
  (string-append
   (dirname (search-path %load-path "libgit2-for-racket.scm"))
   "/.."))

(define-public apple-nix-skel
  ;; TODO: How to get path resolution relative to this file,
  ;; rather than the working directory, at runtime?
  ;; Using a string literal did not work.
  (local-file (string-append repo-root-dir "/apple-nix-skel")
              #:recursive? #t))

(define-public rktLibgit2CommonCmakeFlags.nix
  (plain-file
   "rktLibgit2CommonCmakeFlags.nix"
   (match %common-configure-flags
     (()
      "[]\n")
     ((flag0 . flags)
      (string-concatenate
       (append
        (list (format #f "[ ~s\n" flag0))
        (map (lambda (flag)
               (format #f "  ~s\n" flag))
             flags)
        '("]\n")))))))

(define libgit2-for-racket-with-config.nix
  (plain-file
   "libgit2-for-racket-with-config.nix"
   (string-append
    "import ./libgit2-for-racket.nix rec {\n"
    "  pkgs = import ./nixexprs.tar.xz {};\n"
    "  rktLibgit2Version = \"" %libgit2-version "\";\n"
    "  rktLibgit2Checksum = \"" %libgit2-checksum "\";\n"
    "  rktLibgit2CommonCmakeFlags = ./rktLibgit2CommonCmakeFlags.nix;\n"
    "  rktLibgit2FetchGitUrl = \"" %libgit2-origin-git-url "\";\n"
    "  rktLibgit2Rev = \"" %libgit2-origin-commit "\";\n"
    "}")))

(define-public apple-nix-config
  (file-union
   "apple-nix-config"
   `(("nixexprs.tar.xz" ,pinned-nixpkgs)
     ("libgit2-for-racket-with-config.nix"
      ,libgit2-for-racket-with-config.nix)
     ("rktLibgit2CommonCmakeFlags.nix"
      ,rktLibgit2CommonCmakeFlags.nix))))

(define-public apple-nix-bundle
  (directory-union
   "apple-nix-bundle"
   (list apple-nix-skel apple-nix-config)))

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

