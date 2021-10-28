#!r6rs
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)
(library (libgit2-for-racket common)
  ;; this library needs to work in both Racket and Guile
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
          %common-configure-flags
          ;; platforms
          %all-platforms
          apple-os?
          windows-os?
          os->lib-filename
          os->built-lib-path)
  (import (rnrs base))

  ;; https://github.com/racket/r6rs/pull/6
  (define s+ string-append)
  
  (define %racket-pkg-version "0.0")
  (define %so-version "1.3.0") ;; reserve 1.3 for some day when we can -DDEPRICATE_HARD
  (define %libgit2-version %so-version #;(s+ %so-version ".0"))
  (define %libgit2-checksum
    "0vgpb2175a5dhqiy1iwywwppahgqhi340i8bsvafjpvkw284vazd")
  (define %libgit2-origin-commit
    (s+ "v" %libgit2-version))
  (define %libgit2-origin-git-url
    "https://github.com/libgit2/libgit2")

  ;; https://status.nixos.org/
  (define %nixpkgs-channel "21.05")
  (define %nixpkgs-release-stamp "3916.3b1789322fc") ;; what is the integer before the commit?
  (define %nixpkgs-checksum
    "0a2rw6lqr8yw3i41vkgqr2swxjiazw88l0yxfzixi6hc3xi6s3lv")
  (define %nixpkgs-release
    (s+ "nixos-" %nixpkgs-channel "." %nixpkgs-release-stamp))
  (define %nixpkgs-url
     ;; There is an overview here:
     ;; https://releases.nixos.org/nixos/21.05/nixos-21.05.3916.3b1789322fc
    (s+ "https://releases.nixos.org/nixos/" %nixpkgs-channel "/" %nixpkgs-release "/nixexprs.tar.xz"))
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


  (define-syntax define-platforms
    (syntax-rules ()
      [(_ %all [arch os maybe-triplet] ...)
       (define %all
         `([arch os maybe-triplet ,(s+ (symbol->string 'arch) "-" (symbol->string 'os))]
           ...))]))

  (define-platforms %all-platforms
    ;; #f -> host platform
    [x86_64  linux  #f]
    [x86_64  win32  "x86_64-w64-mingw32"]
    [i386    win32  "i686-w64-mingw32"]
    [x86_64  macosx #f]
    [aarch64 macosx "aarch64-darwin"])

  (define (apple-os? sym)
    ;; e.g. iOS would also be one
    (eq? sym 'macosx))
  (define (windows-os? sym)
    (eq? sym 'win32))
  
(define (os->lib-filename os)
  (case os
    [(win32)
     (s+ "libgit2-" %so-version ".dll")]
    [(macosx)
     (s+ "libgit2." %so-version ".dylib")]
    [else
     (s+ "libgit2.so." %so-version)]))
(define (os->built-lib-path os)
  (if (windows-os? os)
      (s+ "bin/" "libgit2.dll")
      (s+ "lib/" (os->lib-filename os))))
  
  #||#)
