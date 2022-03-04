{
  description = "Flake to generate Racket libgit2 native library packages";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOs";
      repo = "nixpkgs";
      # NOTE: the nixpkgs version used determines MACOSX_VERSION_MIN
      ref = "nixos-21.11";
    };
  };

  outputs = { self, nixpkgs }:
    let

      rkt = {
        pkgVersion = "0.0";
        soVersion = "1.4";
        libgit2 = rec {
          version = "1.4.2";
          src = {
            sha256 = "0xd5w2kzdafipf10sdjmrzzsi12q8rkpcafajwlnmwvrbg6ldvs5";
            rev = "v${version}";
            owner = "libgit2";
            repo = "libgit2";
          };
        };
      };

      # Nix uses Autotools build/host/target terminology

      supportedSystemsForBuild = [ "x86_64-linux" "x86_64-darwin" ];

      hosts = [
        rec {
          nixSystem = "x86_64-linux";
          racketSystem = nixSystem;
          crossAttr = "gnu64";
        }
        {
          nixSystem = "x86_64-w64-mingw32";
          racketSystem = "x86_64-win32";
          crossAttr = "mingwW64";
        }
        {
          nixSystem = "i686-w64-mingw32";
          racketSystem = "i386-win32";
          crossAttr = "mingw32";
        }
        {
          nixSystem = "x86_64-darwin";
          racketSystem = "x86_64-macosx";
          crossAttr = false; # ??? it doesn't seem to be there at all
        }
        rec {
          nixSystem = "aarch64-darwin";
          racketSystem = "aarch64-macosx";
          crossAttr = nixSystem;
        }
      ];
      commonFlags = [
        #"-DDEPRECATE_HARD=ON" # probably should change when updating version!
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

      unixFlags = [ "-DUSE_HTTPS=OpenSSL-Dynamic" ];

      mkWindowsFlags = (nixSystem: [
        "-DDLLTOOL=${nixSystem}-dlltool"
        "-DCMAKE_RC_COMPILER=${nixSystem}-windres"
        # TODO use's racket's openssl
        "-DCMAKE_C_FLAGS=-static-libgcc"
        "-DCMAKE_CXX_FLAGS=-static-libgcc" # -static-libstdc++"
        "-DCMAKE_EXE_LINKER_FLAGS=-static-libgcc" # -static-libstdc++"
        "-DCMAKE_MODULE_LINKER_FLAGS=-static-libgcc" # -static-libstdc++"
      ]);

      # Helper function to generate an attrset
      # genAttrs buildSystemf f -> '{ x86_64-linux = f "x86_64-linux"; ... }'.
      genAttrs = nixpkgs.lib.genAttrs;

    in {

      rkt = rkt;

      packages = genAttrs supportedSystemsForBuild (systemForBuild:
        let
          pkgs = import nixpkgs { system = systemForBuild; };
          src = pkgs.fetchFromGitHub rkt.libgit2.src;
          supportedHosts = builtins.filter ({ crossAttr, ... }:
            pkgs.buildPlatform.isDarwin || ((builtins.isString crossAttr)
              && (!pkgs.pkgsCross.${crossAttr}.hostPlatform.isDarwin))) hosts;
          mkForHost = { nixSystem, racketSystem, crossAttr }:
            let
              pkgsMaybeCross = if nixSystem == systemForBuild then
                pkgs
              else
                pkgs.pkgsCross.${crossAttr};
              hostPlatform = pkgsMaybeCross.hostPlatform;
              lg2 = pkgsMaybeCross.libgit2.overrideAttrs (oldAttrs: {
                version = rkt.libgit2.version;
                src = src;
                patches = [ ];
                buildInputs = [ ];
                cmakeFlags = commonFlags ++ (if hostPlatform.isWindows then
                  mkWindowsFlags nixSystem
                else
                  unixFlags);
              });
              libFileName = if hostPlatform.isWindows then
                "libgit2-${rkt.soVersion}.dll"
              else if hostPlatform.isDarwin then
                "libgit2.${rkt.soVersion}.dylib"
              else
                "libgit2.so.${rkt.soVersion}";
              builtLibPath = if hostPlatform.isWindows then
                "bin/libgit2.dll"
              else
                "lib/${libFileName}";
              racket = "${pkgs.racket-minimal}/bin/racket";
              patchLibCommand = if hostPlatform.isWindows then
                "echo No patch command needed for Windows DLLs."
              else if hostPlatform.isDarwin then
                (let
                  # llvm for llvm-objdump
                  # darwin.binutils-unwrapped for install_name_tool
                  llvmBin = "${pkgs.llvm}/bin";
                  intBin = "${pkgs.darwin.binutils-unwrapped}/bin";
                in ''
                  export PATH=${llvmBin}:${intBin}"''${PATH:+:}$PATH"
                  ${racket} ${self}/patch-darwin-dylib.rkt ${libFileName}
                '')
              else ''
                ${pkgs.patchelf}/bin/patchelf \
                   --set-rpath \$ORIGIN \
                   ${libFileName}
              '';
              packed = pkgs.runCommandLocal "pkg-${racketSystem}" { } ''
                mkdir -p $out/${racketSystem}
                cd $out/${racketSystem}
                cp \
                   ${src}/COPYING \
                   ${src}/AUTHORS \
                   ${src}/git.git-authors \
                   ${src}/docs/changelog.md \
                   .
                cp ${src}/README.md README-libgit2.md
                cp ${lg2}/${builtLibPath} ${libFileName}
                chmod +w ${libFileName}
                ${patchLibCommand}
                chmod -w ${libFileName}
                ${racket} ${self}/generate-info-rkt.rkt \
                          --arch+os ${racketSystem} \
                          --lib-filename ${libFileName} \
                          --pkg-version ${rkt.pkgVersion} \
                          > info.rkt
              '';
            in {
              name = racketSystem;
              lib = lg2;
              packed = packed;
            };
          bundles = builtins.map mkForHost supportedHosts;
        in nixpkgs.lib.lists.foldr nixpkgs.lib.trivial.mergeAttrs {
          libgit2-native-pkgs = pkgs.symlinkJoin {
            name = "libgit2-native-pkgs";
            paths = (nixpkgs.lib.attrsets.catAttrs "packed" bundles);
          };
        } (builtins.map ({ name, lib, packed }: {
          "lib-${name}" = lib;
          "pkg-${name}" = packed;
        }) bundles));

      defaultPackage = genAttrs supportedSystemsForBuild
        (system: self.packages.${system}.libgit2-native-pkgs);
    };
}

