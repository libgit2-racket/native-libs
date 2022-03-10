#lang racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(require racket/file
         racket/path
         racket/cmdline)

(define current-nix-app-run-file (make-parameter #f))
(define current-result-dir (make-parameter #f))

(module+ main
  (let ([run? #f])
    (command-line
     #:once-each
     [("--run") "without this, does nothing"
                (set! run? #t)]
     [("--run-file") run-file "argv[0] for the wrapper command"
                     (current-nix-app-run-file run-file)]
     [("--result") result-dir "the built results to be copied"
                   (current-result-dir result-dir)]
     #:args real-args
     (cond
       [(not run?)
        (eprintf "doing nothing without --run flag\n")]
       [else
        (define (check flag param)
          (unless (param)
            (eprintf "~a: missing required wrapper argument ~v\n"
                     (file-name-from-path (find-system-path 'run-file))
                     flag)
            (exit 1)))
        (check "--run-file" current-nix-app-run-file)
        (check "--result" current-result-dir)
        (define result-dir (current-result-dir))
        (unless (directory-exists? result-dir)
          (raise-arguments-error (try-run-file-who '|<error generating who>|)
                                 "build result directory doesn't exist"
                                 "given" result-dir))
        (define check-existing? #t)
        (command-line
         #:program (current-nix-app-run-file)
         #:argv real-args
         #:usage-help
         "deletes the contents of <dest-dir>, then copies the built packages there"
         #:once-each
         [("--no-check") "don't check existing structure of <dest-dir>"
                         (set! check-existing? #f)]
         #:args (dest-dir)
         (check-and-replace #:result result-dir
                            #:check-existing? check-existing?
                            #:dest (path->complete-path dest-dir)))]))))

(define (check-and-replace #:result result-dir
                           #:check-existing? check-existing?
                           #:dest dest-dir)
  (when check-existing?
    (check-matching-dirs #:result result-dir
                         #:dest dest-dir))
  (directory-delete-files dest-dir)
  (extract-result #:result result-dir
                  #:dest dest-dir))

(define (extract-result #:result result-dir #:dest dest-dir)
  (parameterize ([current-directory result-dir])
    (for ([pth (in-directory)]
          #:unless (memq (file-or-directory-type pth)
                         '(directory directory-link)))
      (define dest-file (build-path dest-dir pth))
      (make-parent-directory* dest-file)
      (copy-file pth dest-file))))

(define (try-run-file-who default)
  (cond
    [(current-nix-app-run-file)
     => (compose1 string->symbol
                  path->string
                  file-name-from-path)]
    [else
     default]))

(define (check-matching-dirs #:result result-dir
                             #:dest dest-dir)
  (define who check-matching-dirs)
  (define in-dest (directory-list dest-dir))
  (define in-result (directory-list result-dir))
  (unless (equal? in-dest in-result)
    (define missing (remove* in-dest in-result))
    (define superfluous (remove* in-result in-dest))
    (apply raise-arguments-error
           (try-run-file-who who)
           "immediate children of destination don't match build result"
           "destination" dest-dir
           "result" result-dir
           `(,@(if (pair? missing)
                   `("missing..." ,missing)
                   null)
             ,@(if (pair? superfluous)
                   `("superfluous..." ,superfluous)
                   null)))))

(define (make-file-writable pth)
  (file-or-directory-permissions
   pth
   (bitwise-ior user-write-bit
                (file-or-directory-permissions pth 'bits))))

(define (directory-delete-files dir)
  (parameterize ([current-directory dir])
    (for ([pth (in-directory)]
          #:unless (equal? #".git"
                           (path->bytes (file-name-from-path pth)))
          #:unless (memq (file-or-directory-type pth)
                         '(directory directory-link)))
      (make-file-writable pth)
      (delete-file pth))))
