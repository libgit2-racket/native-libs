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
    with nixpkgs.lib;
    with attrsets;
    with builtins;
    let

      supportedSystemsForBuild = [ "x86_64-linux" "x86_64-darwin" ];

      genForAllSystems = genAttrs supportedSystemsForBuild;

      rkt = import ./version.nix;

      platforms = import ./nix/platforms.nix;

      builtByPlatform = genForAllSystems (systemForBuild:
        import ./nix/build.nix {
          inherit self nixpkgs systemForBuild rkt platforms;
        });

    in {

      rkt = rkt // { inherit platforms; };

      src = head (catAttrs "src" (attrValues builtByPlatform));

      packages = genForAllSystems
        (systemForBuild: builtByPlatform.${systemForBuild}.packages);

      defaultPackage =
        genForAllSystems (systemForBuild: self.packages.${systemForBuild}.all);
    };
}

