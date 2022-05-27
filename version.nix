{
  pkgVersion = "0.0.0.5"; # info.rkt `(define version ...)`
  breakingChangeLabel = ""; # to change package names for breaking changes
  soVersion = "1.4"; # e.g. for `ffi-lib`
  deprecateHard = true; # controls -DDEPRECATE_HARD=ON
  libgit2 = rec {
    version = "1.4.2"; # IMPORTANT! MUST also update sha256 below.
    src = {
      sha256 = "0xd5w2kzdafipf10sdjmrzzsi12q8rkpcafajwlnmwvrbg6ldvs5";
      rev = "v${version}";
      url = "https://github.com/libgit2/libgit2";
    };
  };
}
