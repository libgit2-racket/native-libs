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

  filterToAttrs = proc:
    mapToAttrs (pkgs: {
      name = pkgs.buildPlatform.system;
      value = pkgs;
    }) (filter proc allPlatforms);
in {

  supportedBuildPlatforms =
    filterToAttrs (pkgs: pkgs.buildPlatform == pkgs.hostPlatform);

  darwinHostPlatforms = filterToAttrs (pkgs: pkgs.hostPlatform.isDarwin);

}
