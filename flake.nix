{
  description = "Flake to generate Racket libgit2 native library packages";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOs";
      repo = "nixpkgs";
      # NOTE: the nixpkgs version used here determines MACOSX_VERSION_MIN
      ref = "nixos-21.11";
    };
  };

  outputs = { self, nixpkgs }:
    let

      resolveCrossAttr = crossAttr:
        # Per <https://nixos.org/manual/nixpkgs/stable/#sec-cross-usage>,
        # this indirection should be unneeded when
        # <https://github.com/NixOS/nixpkgs/issues/34274 is fixed>.
        builtins.getAttr crossAttr nixpkgs.lib.systems.examples;

      darwinLocalSystem = { localSystem = resolveCrossAttr "x86_64-darwin"; };
      pkgsDarwin = import nixpkgs darwinLocalSystem;
      pkgsDarwinCross = import nixpkgs (darwinLocalSystem // {
        crossSystem = resolveCrossAttr "aarch64-darwin";
      });
      pkgsLinux = import nixpkgs { localSystem = resolveCrossAttr "gnu64"; };

      rkt = import "${self}/version.nix";

      darwinHosts = builtins.map (pkgs: rec {
        name = "${pkgs.hostPlatform.qemuArch}-macosx";
        built = pkgs.libgit2.overrideAttrs (oldAttrs: {
          version = rkt.libgit2.version;
          src = pkgs.fetchgit rkt.libgit2.src;
          patches = [ ];
          buildInputs = [ ];
          cmakeFlags = [ ]; # TODO
        });
        extracted = pkgs.runCommand "extracted-${name}" { } ''
          mkdir -p $out/${name}
          cd $out/${name}
          cp ${built}/libgit2.${rkt.soVersion}.dylib .
          echo ${pkgs.buildPlatform.config} > buildPlatform.config.txt
        '';
      }) [ pkgsDarwin pkgsDarwinCross ];
    in rec {
      packages = builtins.listToAttrs (builtins.map (pkgs: {
        name = pkgs.buildPlatform.system;
        value = rec {
          apple = pkgs.symlinkJoin {
            name = "racket-libgit2-apple-bundle-${rkt.pkgVersion}";
            paths = nixpkgs.lib.attrsets.catAttrs "extracted" darwinHosts;
          };
        } // (builtins.listToAttrs (builtins.concatMap
          ({ name, built, extracted }: [
            {
              name = "built-${name}";
              value = built;
            }
            {
              name = "extracted-${name}";
              value = extracted;
            }
          ]) darwinHosts));
      }) [ pkgsLinux pkgsDarwin ]);
    };
}
