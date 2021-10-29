#lang racket
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(require racket/runtime-path
         gregor
         "base.rkt")

(provide apple-run-remote
         current-apple-ssh-info)

(define rsync
  (find-executable-path "rsync"))
(define ssh
  (find-executable-path "ssh"))

(struct ssh-info (host base)
  #:transparent)
(define current-apple-ssh-info
  (let* ([who 'current-apple-ssh-info]
         [parse-ssh-info
          (λ (orig)
            (if (ssh-info? orig)
                orig 
                (match (string-trim
                        (if (string? orig)
                            orig
                            (file->string orig)))
                  [(pregexp  #px"^(?:([^\\s:]+):(\\S+))$"
                             (list _ host base))
                   (ssh-info (string->immutable-string host)
                             (string->immutable-string base))]
                  ;; TODO error reporting
                  )))]
         [current-apple-ssh-info
          (make-parameter (delay/sync
                           (define pth (build-path here "_apple-ssh-host+path"))
                           ;; TODO
                           #;
                           (unless (file-exists? pth)
                             (error ...))
                           (parse-ssh-info pth))
                          (λ (v)
                            (unless (or (ssh-info? v)
                                        (string? v)
                                        (and (path? v)
                                             (complete-path? v)))
                              (raise-argument-error
                               who "(or/c ssh-info? string? (and/c path? complete-path?)"
                               v)))
                          who)]
         [current-apple-ssh-info
          (make-derived-parameter current-apple-ssh-info
                                  values
                                  force)])
    current-apple-ssh-info))

(define (apple-run-remote)
  ;; TODO
  #;(check-git-describe ???)
  (unless (directory-exists? (apple-nix-bundle/))
    (raise-arguments-error 'apple-run-remote
                           "not initialized"
                           "directory" (apple-nix-bundle/)))
  (when (directory-exists? (built-on-apple/))
    (delete-directory/files (built-on-apple/)))
  (define timestamp
    (~t (now)
        ;; Using local timezone for ease of debugging.
        ;; Willful violation of iso8601 because
        ;; ":" is displayed as "/" by Finder on Mac.
        "yyyy-MM-dd'T'HH'h'mm'm'ss's'"))
  (match-define (ssh-info host base)
    (current-apple-ssh-info))
  (define remote-dir
    (++ base "/" timestamp "/"))
  (invoke rsync "-avh" "--progress"
          (path->directory-path (apple-nix-bundle/))
          (++ host ":" remote-dir))
  (invoke ssh host (++ remote-dir "/apple-nix-src/build-all.rkt"))
  (invoke rsync "-avh" "--progress" "--copy-unsafe-links" ;; TODO: is this good enough to skip guix copying things?
          (++ host ":" remote-dir "/built-on-apple/")
          (path->directory-path (built-on-apple/)))
  ;; TODO
  #;(check-git-describe ???))

