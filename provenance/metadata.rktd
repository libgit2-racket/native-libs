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
        (libgit2-rev . "v1.4.2")
        (libgit2-sha256
         .
         "0xd5w2kzdafipf10sdjmrzzsi12q8rkpcafajwlnmwvrbg6ldvs5")
        (libgit2-url . "https://github.com/libgit2/libgit2")
        (libgit2-version . "1.4.2")
        (nixpkgs-source+lock-info
         .
         #hasheq((lastModifiedDate . "20220317041211")
                 (narHash
                  .
                  "sha256-Jcc+vHNDN3KDWuzGNTl3A24ICGovPneJDejiN2t57QI=")
                 (owner . "NixOs")
                 (ref . "nixos-21.11")
                 (repo . "nixpkgs")
                 (rev . "2c66a7a6e036971c4847cca424125f55b9eb0b0b")))
        (pkg-version . "0.0.0.4")
        (self-source-info
         .
         #hasheq((lastModifiedDate . "20220320031609")
                 (narHash
                  .
                  "sha256-OeZG/N5N9vfRLW9RqHRkSLqFBmtxPAJue/2cFKyTDj4=")
                 (rev . "2700a945c6a8f814eb0e17d3116bc141406db69e")))
        (so-version . "1.4"))
