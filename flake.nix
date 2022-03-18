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

      rkt = import "${self}/version.nix";
      build = import "${self}/nix/build.nix" { inherit self nixpkgs rkt; };

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

      darwinHosts =
        builtins.map build.mkDarwinHost [ pkgsDarwin pkgsDarwinCross ];
    in rec {

      packages = builtins.listToAttrs (builtins.map (pkgs: {
        name = pkgs.buildPlatform.system;
        value = build.mkPackagesForBuildPlatform pkgs darwinHosts;
      }) [ pkgsLinux pkgsDarwin ]);

      defaultPackage =
        builtins.mapAttrs (_: builtins.getAttr "guixWithApple") packages;
    };
}
