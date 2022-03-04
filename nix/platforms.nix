# nixSystem:
#   the Nix system tuple (like a GNU tripple, but no vendor or ABI)
# rktPlatform:
#   the normalized `(~a (system-type 'arch) "-" (system-type 'os*))`
#   form used by `raco cross` (which e.g. for Windows is different than
#   the representation used by `setup/matching-platform`)
# crossAttr:
#   the Nix attribute under `pkgsCross` to use
#   (according to <https://nix.dev/tutorials/cross-compilation>,
#   "these attribute names for cross compilation packages have been
#   chosen somewhat freely over the course of time.")
[
  rec {
    nixSystem = "x86_64-linux";
    rktPlatform = nixSystem;
    crossAttr = "gnu64";
  }
  {
    nixSystem = "x86_64-w64-mingw32";
    rktPlatform = "x86_64-win32";
    crossAttr = "mingwW64";
  }
  {
    nixSystem = "i686-w64-mingw32";
    rktPlatform = "i386-win32";
    crossAttr = "mingw32";
  }
  {
    nixSystem = "x86_64-darwin";
    rktPlatform = "x86_64-macosx";
    crossAttr = false; # ??? it doesn't seem to be there at all
  }
  rec {
    nixSystem = "aarch64-darwin";
    rktPlatform = "aarch64-macosx";
    crossAttr = nixSystem;
  }
]
