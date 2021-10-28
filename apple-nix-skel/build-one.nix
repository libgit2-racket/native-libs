{ pkgsCrossAttr ? null
}:
let
  stdArgs = import ./args.nix;
  crossPkgs = if pkgsCrossAttr == null then
    { }
  else {
    pkgs = builtins.getAttr pkgsCrossAttr stdArgs.pkgs.pkgsCross;
  };
in
import ./libgit2-for-racket.nix (stdArgs // crossPkgs)
