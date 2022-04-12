{ nixpkgs }:
with builtins;
let

  # Per <https://nixos.org/manual/nixpkgs/stable/#sec-cross-usage>,
  # this indirection should be unneeded when
  # <https://github.com/NixOS/nixpkgs/issues/34274 is fixed>.

  allPlatforms = map (import nixpkgs) (with nixpkgs.lib.systems.examples; [
    { localSystem = gnu64; }
    { localSystem = x86_64-darwin; }
    {
      localSystem = x86_64-darwin;
      crossSystem = aarch64-darwin;
    }
  ]);

  inherit (import ./lib.nix { inherit (nixpkgs) lib; }) mapToAttrs;

  filterToAttrs = { choose, name }:
    mapToAttrs (pkgs: {
      name = name pkgs;
      value = pkgs;
    }) (filter choose allPlatforms);
in rec {

  supportedBuildPlatforms = filterToAttrs {
    choose = pkgs: pkgs.buildPlatform == pkgs.hostPlatform;
    name = pkgs: pkgs.buildPlatform.system;
  };

  supportedGuixBuildPlatforms = filterToAttrs rec {
    choose = pkgs:
      ((!pkgs.hostPlatform.isDarwin)
        && (hasAttr (name pkgs) supportedBuildPlatforms));
    name = pkgs: pkgs.buildPlatform.system;
  };

  darwinHostPlatforms = filterToAttrs {
    choose = pkgs: pkgs.hostPlatform.isDarwin;
    name = pkgs: pkgs.hostPlatform.system;
  };

}
