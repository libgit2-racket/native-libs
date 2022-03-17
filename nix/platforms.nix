# rktPlatform:
#   the normalized `(~a (system-type 'arch) "-" (system-type 'os*))`
#   form used by `raco cross` (which e.g. for Windows is different than
#   the representation used by `setup/matching-platform`)
# supportedForBuild:
#   if present/true, can be used as the build (vs. host/target) platform
# nixAttr:
#   the Nix attribute under `nixpkgs.lib.examples` to use
#   (According to <https://nix.dev/tutorials/cross-compilation>,
#   "these attribute names for cross compilation packages have been
#   chosen somewhat freely over the course of time.")
#   (It would be nice not to need this, and per
#   <https://nixos.org/manual/nixpkgs/stable/#sec-cross-usage>,
#   it should be obsolete when
#   <https://github.com/NixOS/nixpkgs/issues/34274> is fixed,
#   but, for now, trying to do without causes problems: at a minimum,
#   many rebuilds when using a remote x86_64-macosx.)
[
  rec {
    rktPlatform = "x86_64-linux";
    guixSystem = rktPlatform;
    supportedForBuild = true;
  }
  rec {
    rktPlatform = "x86_64-win32";
    guixSystem = "x86_64-w64-mingw32";
  }
  rec {
    rktPlatform = "i386-win32";
    guixSystem = "i686-w64-mingw32";
  }
  {
    rktPlatform = "x86_64-macosx";
    nixAttr = "x86_64-darwin";
    supportedForBuild = true;
  }
  {
    rktPlatform = "aarch64-macosx";
    nixAttr = "aarch64-darwin";
  }
]
