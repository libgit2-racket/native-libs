{ pkgs
, rktLibgit2Version
, rktLibgit2Src
, rktLibgit2CommonFlags
}:

pkgs.libgit2.overrideAttrs (oldAttrs: rec {
  version = rktLibgit2Version;

  src = rktLibgit2Src;

  cmakeFlags = rktLibgit2CommonFlags ++ [
    "-DUSE_HTTPS=OpenSSL-Dynamic"
  ];

  buildInputs = [];

})
