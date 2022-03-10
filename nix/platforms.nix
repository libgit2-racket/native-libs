# rktPlatform:
#   the normalized `(~a (system-type 'arch) "-" (system-type 'os*))`
#   form used by `raco cross` (which e.g. for Windows is different than
#   the representation used by `setup/matching-platform`)
# supportedForBuild:
#   if present/true, can be used as the build (vs. host/target) platform
# crossAttr:
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
    crossAttr = "gnu64";
    supportedForBuild = true;
  }
  {
    rktPlatform = "x86_64-win32";
    crossAttr = "mingwW64";
  }
  {
    rktPlatform = "i386-win32";
    crossAttr = "mingw32";
  }
  rec {
    rktPlatform = "x86_64-macosx";
    crossAttr = "x86_64-darwin";
    supportedForBuild = true;
  }
  rec {
    rktPlatform = "aarch64-macosx";
    crossAttr = "aarch64-darwin";
  }
]
