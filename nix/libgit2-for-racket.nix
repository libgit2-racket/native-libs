{ pkgs ? import <nixpkgs> {}
, rktLibgit2Version
, rktLibgit2Checksum ? "0vgpb2175a5dhqiy1iwywwppahgqhi340i8bsvafjpvkw284vazd"
, rktLibgit2CommonCmakeFlags ? []
, rktLibgit2FetchGitUrl ? "https://github.com/libgit2/libgit2"
, rktLibgit2Rev ? "v${rktLibgit2Version}"
}:

pkgs.libgit2.overrideAttrs (oldAttrs: rec {
  version = rktLibgit2Version;

  src = pkgs.fetchgit {
    url = rktLibgit2FetchGitUrl;
    rev = rktLibgit2Rev;
    sha256 = rktLibgit2Checksum;
  };

  cmakeFlags = rktLibgit2CommonCmakeFlags ++ [
    "-DTHREADSAFE=ON" # placeholder for platform-specific flags
  ];

})
