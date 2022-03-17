(define-module (old-stable-libc)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages)
  #:use-module (gnu packages base) ;; glibc
  #:use-module (gnu packages commencement) ;; make-gcc-toolchain
  #:use-module (gnu packages gcc)
  #:export (package-with-old-stable-libc))

;; Support for building GNU/Linux binaries to run without Guix.

;; Binaries that dynamically link to glibc can run against newer
;; versions than they were linked against, but not older ones. Given
;; Guix's release cadence, using its default glibc would create
;; binaries only able to run on quite recent distributions. This is
;; not an issue for cross-compiled binaries for non-GNU/Linux systems.

;; Racket's generic Linux binary distribution is currently built on
;; Debian 9 (Stretch). (Technically, in Debian terms, that would be an
;; oldoldstable libc.)

;; glibc : distro
;; -----------------------------
;;  2.31 : Debian 11 (Bullseye)
;;  2.31 : Ubuntu 20.04 (Focal)
;;  2.28 : Debian 10 (Buster)
;;  2.27 : Ubuntu 18.04 (Bionic)
;;  2.24 : Debian 9 (Stretch)

;; See, for example:
;;   - https://packages.debian.org/stretch/glibc-source
;;   - https://launchpad.net/ubuntu/bionic/+source/glibc

;; Using an inferior doesn't quite work: it seems an inferior-package
;; is not sufficiently equivalent to a package.

(define (package-with-old-stable-libc pkg)
  (package-with-c-toolchain
   pkg
   ;; FIXME: gcc-glibc-2.31-toolchain
   `(("gcc-toolchain" ,gcc-glibc-2.31-toolchain))))

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
