(define-module (apple-libgit2-for-racket-via-nix)
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (ice-9 match)
  #:use-module (libgit2-for-racket)
  #:use-module (racket-libgit2-build-constants))

;; nix show-config --json

;; {"allowed-impure-host-deps":{"description":"Which prefixes to allow derivations to ask for access to (primarily for Darwin).","value":["/System/Library","/bin/sh","/dev","/usr/lib"]}}

(define-public pinned-nixpkgs
  (origin
    (method url-fetch)
    (uri %nixpkgs-url)
    (sha256 (base32 %nixpkgs-checksum))))


(define-public repo-root-dir
  (string-append
   (dirname (search-path %load-path "libgit2-for-racket.scm"))
   "/.."))

(define-public apple-nix-skel
  ;; TODO: How to get path resolution relative to this file,
  ;; rather than the working directory, at runtime?
  ;; Using a string literal did not work.
  (local-file (string-append repo-root-dir "/apple-nix-skel")
              #:recursive? #t))
#|
(define-public rktLibgit2CommonCmakeFlags.nix
  (plain-file
   "rktLibgit2CommonCmakeFlags.nix"
   (match %common-configure-flags
     (()
      "[]\n")
     ((flag0 . flags)
      (string-concatenate
       (append
        (list (format #f "[ ~s\n" flag0))
        (map (lambda (flag)
               (format #f "  ~s\n" flag))
             flags)
        '("]\n")))))))

(define default.nix
  (plain-file
   "default.nix"
   (string-append
    "import ./libgit2-for-racket.nix rec {\n"
    #;"  pkgs = import ./nixexprs.tar.xz {};\n"
    "  rktLibgit2Version = \"" %libgit2-version "\";\n"
    "  rktLibgit2Checksum = \"" %libgit2-checksum "\";\n"
    "  rktLibgit2CommonCmakeFlags = import ./rktLibgit2CommonCmakeFlags.nix;\n"
    "  rktLibgit2FetchGitUrl = \"" %libgit2-origin-git-url "\";\n"
    "  rktLibgit2Rev = \"" %libgit2-origin-commit "\";\n"
    "}")))
|#

(define (extracted-.tar.xz-directory name pth.tar.xz)
  (computed-file
   name
   (with-imported-modules
       `((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (mkdir-p #$output)
         (chdir #$output)
         (setenv "PATH" #+(file-append xz "/bin"))
         (invoke #+(file-append tar "/bin/tar")
                 "-xvf" #+pth.tar.xz
                 "--strip-components=1")))))

(define-public apple-nix-config
  (file-union
   "apple-nix-config"
   `(("nixpkgs" ,(extracted-.tar.xz-directory
                  "nixpkgs"
                  pinned-nixpkgs))
     ("src" ,(extracted-.tar.xz-directory
              "libgit2-src"
              libgit2-origin))
     #;
     ("default.nix"
     ,default.nix)
     #;
     ("rktLibgit2CommonCmakeFlags.nix"
      ,rktLibgit2CommonCmakeFlags.nix))))

(define-public apple-nix-bundle
  (directory-union
   "apple-nix-bundle"
   (list apple-nix-skel apple-nix-config)))
