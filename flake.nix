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

      build = import ./nix/build.nix { inherit self nixpkgs; };

      resolveCrossAttr = crossAttr:
        # Per <https://nixos.org/manual/nixpkgs/stable/#sec-cross-usage>,
        # this indirection should be unneeded when
        # <https://github.com/NixOS/nixpkgs/issues/34274 is fixed>.
        builtins.getAttr crossAttr nixpkgs.lib.systems.examples;

      platformsForBuild = builtins.concatMap
        ({ rktPlatform, crossAttr, ... }@spec:
          nixpkgs.lib.lists.optional (spec.supportedForBuild or false) rec {
            crossAttrForBuild = crossAttr;
            localSystem = resolveCrossAttr crossAttr;
            pkgs = import nixpkgs { inherit localSystem; };
          }) (import ./nix/platforms.nix);

      darwinPlatformForBuild = let
        found = builtins.filter ({ pkgs, ... }: pkgs.buildPlatform.isDarwin)
          platformsForBuild;
      in assert nixpkgs.lib.assertMsg (builtins.length found >= 1)
        "no Darwin platform specified as supportedForBuild";
      builtins.head found;

      getPkgsForTarget = { crossAttrForBuild, localSystem, pkgs }:
        { rktPlatform, crossAttr, ... }:
        let
          crossSystem = resolveCrossAttr crossAttr;
          crossPkgs = import nixpkgs { inherit localSystem crossSystem; };
        in if crossAttr == crossAttrForBuild then
          pkgs
        else if (!crossPkgs.hostPlatform.isDarwin)
        || pkgs.buildPlatform.isDarwin then
          crossPkgs
        else if crossAttr == darwinPlatformForBuild.crossAttrForBuild then
          darwinPlatformForBuild.pkgs
        else
          import nixpkgs {
            inherit crossSystem;
            localSystem = darwinPlatformForBuild.localSystem;
          };

    in { };
}

#   with nixpkgs.lib;
#   with attrsets;
#   with builtins;
#   let
#
#     rkt = import ./version.nix;
#
#     platforms = import ./nix/platforms.nix;
#
#     builtByPlatform = genForAllSystems (systemForBuild:
#      import ./nix/build.nix {
#       inherit self nixpkgs systemForBuild rkt platforms;
#     });

# in {

#   rkt = rkt // { inherit platforms; };

#  src = head (catAttrs "src" (attrValues builtByPlatform));

#  packages = genForAllSystems
#   (systemForBuild: builtByPlatform.${systemForBuild}.packages);

# defaultPackage =
#   genForAllSystems (systemForBuild: self.packages.${systemForBuild}.all);
# };
#}
