(use-modules (guix)
             (guix git-download)
             (gnu packages version-control))
(package
  (inherit libgit2)
  (name "libgit2") ;; to bind it in origin
  (version "1.3.0")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/libgit2/libgit2")
                  (commit (string-append "v" version))))
            (sha256
             (base32
              "0vgpb2175a5dhqiy1iwywwppahgqhi340i8bsvafjpvkw284vazd"))
            (patches
             (origin-patches (package-source libgit2)))
            (file-name (git-file-name name version))))
  ;;(outputs '("out"))
  (inputs `())
  (propagated-inputs `())
  (arguments
   (substitute-keyword-arguments (package-arguments libgit2)
     ((#:configure-flags _ '())
      (let* ((target (%current-target-system))
             (windows? (and target (string-contains target "mingw"))))
        `(list "-DREGEX_BACKEND=builtin"
               "-DUSE_HTTP_PARSER=builtin"
               "-DUSE_BUNDLED_ZLIB=ON"
               "-DUSE_NTLMCLINT=OFF"
               "-DUSE_SSH=OFF"
               ,@(if windows?
                     (list (string-append "-DDLLTOOL=" target "-dlltool")
                           (string-append "-DCMAKE_RC_COMPILER="
                                          target "-windres")
                           ;; TODO use's racket's openssl
                           )
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
             (setenv "CLAR_TMP" "my-clar-tmp"))))))))

