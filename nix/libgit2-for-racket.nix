{ pkgs ? import <nixpkgs> {}
}:

pkgs.libgit2.overrideAttrs (oldAttrs: rec {
  version = "1.3.0";

  src = fetchFromGithub {
    owner = "libgit2";
    repo = "libgit2";
    rev = "v${version}";
    sha256 = "0vgpb2175a5dhqiy1iwywwppahgqhi340i8bsvafjpvkw284vazd";
  };

});
