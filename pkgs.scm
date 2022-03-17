(define-module (pkgs))
(use-modules (guix utils)
             (guix gexp)
             (guix git-download)
             (guix packages)
             (ice-9 match)
             (srfi srfi-26)
             (gnu packages)
             (gnu packages base) ;; glibc
             (gnu packages commencement) ;; make-gcc-toolchain
             (gnu packages gcc))

;; glibc : distro
;; -----------------------------
;;  2.31 : Debian 11 (Bullseye)
;;  2.31 : Ubuntu 20.04 (Focal)
;;  2.28 : Debian 10 (Buster)
;;  2.27 : Ubuntu 18.04 (Bionic)
;;  2.24 : Debian 9 (Stretch)

;; Debian 9 (Stretch), which is used to build Racket's released
;; generic Linux binaries, uses glibc 2.24:
;; <https://packages.debian.org/stretch/glibc-source>.
;; For comparison, Ubuntu 18.04 (Bionic) uses glibc 2.27:
;; <https://launchpad.net/ubuntu/bionic/+source/glibc>.
;; By using it, our binary should be able to run everywhere
;; that the Racket distribution can run.

;; Using an inferior doesn't quite work: it seems an inferior-package
;; is not sufficiently equivalent to a package.

(define-public gcc-glibc-2.31-toolchain
  (make-gcc-toolchain gcc glibc-2.31))

(define-public guix-for-glibc-2.24
  (origin
    (method git-fetch)
    (uri
     (git-reference
      (url "https://git.savannah.gnu.org/git/guix.git")
      (commit
       ;; glibc-2.24 was removed in ebd1ba713cbefc9ad5dac609255e1344a328e360
       ;; This commit is the parent of that one:
       "ccc1d743a64fd71bee1a27f1f495978989b41126")))
    (sha256
     (base32 "137g9yrkhm1dnfswmpralk4bdkr4j8b6xq4i5vcgrq671kr706h7"))
    (file-name (git-file-name "guix" "for-glibc-2.24"))))

(define-public patches-for-glibc-2.24
  (let ((patch-dir (file-append guix-for-glibc-2.24
                                "/gnu/packages/patches/")))
    (map (cut file-append patch-dir <>)
         '("glibc-ldd-x86_64.patch"
           "glibc-versioned-locpath.patch"
           "glibc-vectorized-strcspn-guards.patch"
           "glibc-CVE-2015-5180.patch"
           "glibc-CVE-2017-1000366-pt1.patch"
           "glibc-CVE-2017-1000366-pt2.patch"
           "glibc-CVE-2017-1000366-pt3.patch"))))

(define-public glibc-2.24
  ;; currently broken ...
  (package
    ;; per comment on glibc-2.30, it needs gcc 8, not gcc 10
    (inherit glibc-2.30)
    (version "2.24")
    (source
     (origin
       (inherit (package-source glibc-2.30))
       (uri (string-append "mirror://gnu/glibc/glibc-"
                           version ".tar.xz"))
       (sha256
        (base32
         "1lxmprg9gm73gvafxd503x70z32phwjzcy74i0adfi6ixzla7m4r"))
       (patches (append
                 #;(search-patches "glibc-skip-c++.patch")
                 patches-for-glibc-2.24))))))

(define-public gcc-glibc-2.24-toolchain
  (make-gcc-toolchain gcc glibc-2.24))

gcc-glibc-2.31-toolchain
