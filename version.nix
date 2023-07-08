{
  pkgVersion = "0.2"; # info.rkt `(define version ...)`
  breakingChangeLabel = ""; # to change package names for breaking changes
  soVersion = "1.4"; # e.g. for `ffi-lib`
  deprecateHard = true; # controls -DDEPRECATE_HARD=ON
  libgit2 = rec {
    version = "1.4.3"; # IMPORTANT! MUST also update sha256 below.
    # One way: use a bogus sha256, get the correct one from the error message,
    # and convert it from the SRI representation with `nix hash to-base32`.
    src = {
      sha256 = "02x1a4zrzpzjd0yxnsi8njh5hgihc1iy1v4r0fnl8m4ckcgp6x2s";
      rev = "v${version}";
      url = "https://github.com/libgit2/libgit2";
    };
  };
}
