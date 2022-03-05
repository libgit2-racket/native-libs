{ deprecateHard ? false, ... }: rec {
  forHostPlatform = { isWindows, config, ... }:
    common ++ (if isWindows then mkWindows config else unix);

  common = (if deprecateHard then [ "-DDEPRECATE_HARD=ON" ] else [ ]) ++ [
    "-DREGEX_BACKEND=builtin" # maybe via Racket, one day?
    # re libssh2, see:
    #   - https://github.com/libgit2/libgit2/issues/5640#issuecomment-699704636
    #   - https://github.com/libgit2/libgit2sharp/issues/1809#issuecomment-659460624
    #   - https://github.com/libgit2/libgit2/pull/5507/commits/de499cc21335ac9ffe44e6c1825ff7417b45a895
    #   - https://github.com/libgit2/libgit2/pull/5507/commits/d6dacfb25f69ff348646d740d768f277db017e3d
    "-DUSE_SSH=OFF"
    "-DUSE_HTTP_PARSER=builtin"
    "-DUSE_BUNDLED_ZLIB=ON" # does Racket have one already?
    "-DUSE_NTLMCLINT=OFF"
  ];

  unix = [ "-DUSE_HTTPS=OpenSSL-Dynamic" ];

  mkWindows = (nixSystem: [
    "-DDLLTOOL=${nixSystem}-dlltool"
    "-DCMAKE_RC_COMPILER=${nixSystem}-windres"
    # TODO use's racket's openssl
    "-DCMAKE_C_FLAGS=-static-libgcc"
    "-DCMAKE_CXX_FLAGS=-static-libgcc" # -static-libstdc++"
    "-DCMAKE_EXE_LINKER_FLAGS=-static-libgcc" # -static-libstdc++"
    "-DCMAKE_MODULE_LINKER_FLAGS=-static-libgcc" # -static-libstdc++"
  ]);
}
