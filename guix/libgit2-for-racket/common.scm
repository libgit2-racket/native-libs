#!r6rs
(library (libgit2-for-racket common)
  (export %racket-pkg-version
          %so-version
          %libgit2-version
          %libgit2-checksum
          %libgit2-origin-commit
          %libgit2-origin-git-url
          ;; Nix
          %nixpkgs-channel
          %nixpkgs-release
          %nixpkgs-checksum
          %nixpkgs-url
          ;; configure flags
          %common-configure-flags)
  (import (rnrs base))

  (define %racket-pkg-version "0.0")
  (define %so-version "1.3")
  (define %libgit2-version (string-append %so-version ".0"))
  (define %libgit2-checksum
    "0vgpb2175a5dhqiy1iwywwppahgqhi340i8bsvafjpvkw284vazd")
  (define %libgit2-origin-commit
    (string-append "v" %libgit2-version))
  (define %libgit2-origin-git-url
    "https://github.com/libgit2/libgit2")

  ;; https://status.nixos.org/
  (define %nixpkgs-channel "21.05")
  (define %nixpkgs-release-stamp "3916.3b1789322fc") ;; what is the integer before the commit?
  (define %nixpkgs-checksum
    "0a2rw6lqr8yw3i41vkgqr2swxjiazw88l0yxfzixi6hc3xi6s3lv")
  (define %nixpkgs-release
    (string-append "nixos-" %nixpkgs-channel "." %nixpkgs-release-stamp))
  (define %nixpkgs-url
    (string-append
     ;; TODO: check this
     ;; https://releases.nixos.org/nixos/21.05/nixos-21.05.3916.3b1789322fc
     "https://releases.nixos.org/nixos/" %nixpkgs-channel "/" %nixpkgs-release "/nixexprs.tar.xz"))
  (define %common-configure-flags
    `("-DREGEX_BACKEND=builtin" ;; maybe via Racket, one day?
      ;; re libssh2, see:
      ;;   - https://github.com/libgit2/libgit2/issues/5640#issuecomment-699704636
      ;;   - https://github.com/libgit2/libgit2sharp/issues/1809#issuecomment-659460624
      ;;   - https://github.com/libgit2/libgit2/pull/5507/commits/de499cc21335ac9ffe44e6c1825ff7417b45a895
      ;;   - https://github.com/libgit2/libgit2/pull/5507/commits/d6dacfb25f69ff348646d740d768f277db017e3d
      "-DUSE_SSH=OFF"
      "-DUSE_HTTP_PARSER=builtin"
      "-DUSE_BUNDLED_ZLIB=ON" ;; does Racket have one already?
      "-DUSE_NTLMCLINT=OFF"))

  (define (apple-os? sym)
    (eq? sym 'macosx))
  
  #||#)
