#hasheq((breaking-change-label . "")
        (cfg-flags-common
         .
         ("-DDEPRECATE_HARD=ON"
          "-DREGEX_BACKEND=builtin"
          "-DUSE_SSH=OFF"
          "-DUSE_HTTP_PARSER=builtin"
          "-DUSE_BUNDLED_ZLIB=ON"
          "-DUSE_NTLMCLINT=OFF"))
        (cfg-flags-unix . ("-DUSE_HTTPS=OpenSSL-Dynamic"))
        (cfg-flags-windows
         .
         ("-DCMAKE_RC_COMPILER=${arch}-w64-mingw32-windres"
          "-DDLLTOOL=${arch}-w64-mingw32-dlltool"
          "-DCMAKE_C_FLAGS=-static-libgcc"
          "-DCMAKE_CXX_FLAGS=-static-libgcc"
          "-DCMAKE_EXE_LINKER_FLAGS=-static-libgcc"
          "-DCMAKE_MODULE_LINKER_FLAGS=-static-libgcc"))
        (deprecate-hard . #t)
        (libgit2-rev . "v1.4.3")
        (libgit2-sha256
         .
         "02x1a4zrzpzjd0yxnsi8njh5hgihc1iy1v4r0fnl8m4ckcgp6x2s")
        (libgit2-url . "https://github.com/libgit2/libgit2")
        (libgit2-version . "1.4.3")
        (nixpkgs-source+lock-info
         .
         #hasheq((lastModifiedDate . "2022-05-22T14:30:24Z")
                 (narHash
                  .
                  "sha256-klSCYMpR4TqWYoTD/xZ2qM9UIPRFC6pK+S/kJuVLbFw=")
                 (owner . "NixOs")
                 (ref . "nixos-21.11")
                 (repo . "nixpkgs")
                 (rev . "06db2e2197401b74fcf82d4e84be15b0b5851c7b")))
        (pkg-version . "0.2")
        (self-source-info
         .
         #hasheq((lastModifiedDate . "2023-07-08T16:47:32Z")
                 (narHash
                  .
                  "sha256-ITpLVw5Q7NoDCNcybRn1uDn0cE8Ae/J4XwHBnWbsZ/o=")
                 (rev . "0c719ea327f30a1e5e7abc13ddfdd2ad1b065316")))
        (so-version . "1.4")
        (this-package
         .
         #hasheq((built . #hasheq((by . "Nix") (on . "x86_64-apple-darwin")))
                 (packed . #hasheq((by . "Guix") (on . "x86_64-linux"))))))
