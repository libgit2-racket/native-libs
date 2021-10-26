(define-module (libgit2-for-racket apple-via-nix)
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (ice-9 match)
  #:use-module (libgit2-for-racket)
  #:use-module (libgit2-for-racket common))

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
                  %nixpkgs-release
                  pinned-nixpkgs))
     ("src" ,(extracted-.tar.xz-directory
              "libgit2-src"
              libgit2-origin))
     ("args.nix" ,(plain-file
                   "args.nix"
                   (string-append
                    "{ pkgs = ./nixpkgs;\n"
                    "  rktLibgit2Version = \"" %libgit2-version "\";\n"
                    "  rktLibgit2Src = ./src;\n"
                    "  rktLibgit2CommonFlags = [\n"
                    (string-concatenate
                     (map (lambda (flag)
                            (format #f "    ~s\n" flag))
                          %common-configure-flags))
                    "  ];\n"
                    "};\n"))))))

(define-public apple-nix-bundle
  (directory-union
   "apple-nix-bundle"
   (list apple-nix-skel apple-nix-config)))
