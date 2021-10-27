{ pkgsCrossAttr ? null }:
let
  stdArgs = import ./args.nix;
  mkPkg = import ./libgit2-for-racket.nix;
in if (pkgsCrossAttr != null) then
  mkPkg (builtins.getAttr pkgsCrossAttr stdArgs.pkgs.pkgsCross)
else
  mkPkg stdArgs
