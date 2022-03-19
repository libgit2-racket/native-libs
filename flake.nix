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
    with nixpkgs.lib;
    let

      rkt = import "${self}/version.nix";
      build = import "${self}/nix/build.nix" { inherit self nixpkgs rkt; };
      inherit (import "${self}/nix/lib.nix" { inherit (nixpkgs) lib; })
        mapToAttrs concatAttrsSuffixed;
      inherit (import "${self}/nix/platforms.nix" { inherit nixpkgs; })
        supportedBuildPlatforms darwinHostPlatforms;

      applePlatformsExtracted =
        # result is keyed by the Racket platform name
        attrsets.mapAttrs' build.mkExtractedDarwin darwinHostPlatforms;

      basePackages = builtins.mapAttrs (_: pkgs:
        build.mkBasePackages { inherit pkgs applePlatformsExtracted; })
        supportedBuildPlatforms;

      commands = builtins.mapAttrs (system: pkgs:
        import "${self}/nix/commands.nix" {
          inherit nixpkgs pkgs;
          basePackages = basePackages.${system};
        }) supportedBuildPlatforms;

    in rec {

      packages =
        builtins.mapAttrs (system: base: base // commands.${system}.packages)
        basePackages;

      defaultPackage =
        builtins.mapAttrs (_: builtins.getAttr "guix-with-apple") packages;

      apps = builtins.mapAttrs (_: { apps, ... }: apps) commands;

      defaultApp = builtins.mapAttrs (_: builtins.getAttr "time-machine") apps;
    };
}
