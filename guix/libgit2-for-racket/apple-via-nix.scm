(define-module (libgit2-for-racket apple-via-nix)
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (libgit2-for-racket)
  #:use-module (libgit2-for-racket common))

;; nix show-config --json

;; {"allowed-impure-host-deps":{"description":"Which prefixes to allow derivations to ask for access to (primarily for Darwin).","value":["/System/Library","/bin/sh","/dev","/usr/lib"]}}

(define-public pinned-nixpkgs
  (origin
    (method url-fetch)
    (uri %nixpkgs-url)
    (sha256 (base32 %nixpkgs-checksum))))

(define (extract-.tar.xz-directory-gexp pth.tar.xz)
   (with-imported-modules
       `((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (mkdir-p #$output)
         (chdir #$output)
         (setenv "PATH" #+(file-append xz "/bin"))
         (invoke #+(file-append tar "/bin/tar")
                 "-xvf" #+pth.tar.xz
                 "--strip-components=1"))))

;; The pinned-nixpkgs tarball contains a symlink "nixpkgs" to ".".
;; This cycle causes various complications. Some tools (e.g. at
;; least some versions of scp) will follow the cycle into an
;; infinate loop. It is especially tricky because, when we build
;; up the bundle directory, it has lots of symlinks to store files
;; that we DO want to follow, so we can't just use a naïve
;; don't-follow-symlinks option.
;;
;; As a workaround, when we unpack the tarball, we delete the nixpkgs
;; symlink. Once we've built up the bundle directory, we recursively
;; copy everything into a new directory (so there are no remaining
;; symlinks we'd need to follow into the store), then create a new
;; "nixpkgs" -> "." symlink. Then, at least, the directory can be
;; handled by any tool with a don't-follow-symlinks mode.

(define-public nixpkgs-sans-symlink-cycle
  (computed-file
   (string-append %nixpkgs-release "-sans-symlink-cycle")
   (with-imported-modules
       `((guix build utils)
         (srfi srfi-34)
         (srfi srfi-35))
     #~(begin
         (use-modules (guix build utils)
                      (srfi srfi-34)
                      (srfi srfi-35))
         #$(extract-.tar.xz-directory-gexp pinned-nixpkgs)
         (unless (equal? "." (readlink "nixpkgs"))
           (raise
            (condition
             (&error)
             (&message
              (message "nixpkgs did not contain a symlink to itself")))))
         (delete-file "nixpkgs")))))

(define-public apple-nix-computed
  (file-union
   "apple-nix-computed"
   `(("nixpkgs" ,nixpkgs-sans-symlink-cycle)
     ("src" ,(computed-file
              "libgit2-src"
              (extract-.tar.xz-directory-gexp
               libgit2-origin)))
     ("platforms.rktd" ,(scheme-file
                         "platforms.rktd"
                         (filter-map
                          (match-lambda
                            ((arch os pkgsCrossAttr arch-os)
                             (and (apple-os? os)
                                  (list pkgsCrossAttr
                                        (os->built-lib-path os)
                                        arch-os
                                        (os->lib-filename os)))))
                          %all-platforms)))
     ("args.nix" ,(plain-file
                   "args.nix"
                   (string-append
                    "{ pkgs = import ./nixpkgs {};\n"
                    "  rktLibgit2Version = \"" %libgit2-version "\";\n"
                    "  rktLibgit2Src = ./src;\n"
                    "  rktLibgit2CommonFlags = [\n"
                    (string-concatenate
                     (map (lambda (flag)
                            (format #f "    ~s\n" flag))
                          %common-configure-flags))
                    "  ];\n"
                    "}\n"))))))

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

(define-public apple-nix-src-pre-symlink-fix
  (directory-union
   "apple-nix-src-pre-symlink-fix"
   (list apple-nix-skel
         apple-nix-computed)))

(define-public apple-nix-src
  (computed-file
   "apple-nix-src"
   (with-imported-modules `((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (copy-recursively
          #$apple-nix-src-pre-symlink-fix
          #$output
          #:follow-symlinks? #t)
         (chdir #$output)
         (chdir "nixpkgs")
         (symlink "." "nixpkgs")))))
