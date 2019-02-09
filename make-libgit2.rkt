#lang racket/base

(require setup/matching-platform
         racket/system
         racket/file
         racket/port
         racket/promise
         racket/pretty
         racket/match
         racket/runtime-path)

(define version "28")

(define supported-platforms
  ;; (listof (non-empty-listof string?))
  ;; Strings represent platforms in the sense of `matching-platform?`.
  `(["x86_64-linux"]
    ["x86_64-macosx"]))

;; to cross-compile i386-linux:
;; sudo apt install libc6-dev-i386
;; sudo apt install libssl-dev ;; could we just use Racket's ???
;; add this to both cmake calls:
;; -DCMAKE_C_FLAGS=-m32
;; cross-compiling on Linux seems to be a problem for ctest, though

;; SSH disabled on Mac, Linux right now
;; iconv disabled on Linux right now ;; this is w/ Racket, right ???

(module+ main
  (require racket/cmdline)
  (let ([lib? #t]
        [info? #t]
        [force? #f])
    (command-line
     #:usage-help "Build the libgit2 native library package for the current platform"
     #:once-any
     [("--clean-first") "delete build artifacts before making"
                        (clean)]
     [("--clean") "like \"--clean-first\", but exit without making"
                  (clean)
                  (exit 0)]
     #:once-each
     [("--no-lib") "skip making the shared library binary"
                   (set! lib? #f)]
     [("--no-info") "skip making the \"info.rkt\" file"
                    (set! info? #f)]
     [("--force") "assume all sources are outdated"
                  (set! force? #t)]
     #:args ()
     (make #:lib? lib? #:info? info? #:force? force?))))

(define who 'make-libgit2.rkt)

(define so-name
  (format (match (system-type)
            ['macosx "libgit2.~a.dylib"]
            ['windows (error who "TODO: windows")]
            [_ "libgit2.so.~a"])
          version))

(define-runtime-path src "src")

(match-define-values [here _ _]
  (split-path src))

(define build-base-dir
  (build-path here "build"))

(define (clean)
  (delete-directory/files build-base-dir
                          #:must-exist? #f))

(define .src-status
  (build-path here ".src-status"))

(define get-current-src-status
  (let ([pr (delay
              (with-output-to-string
                (λ ()
                  (parameterize ([current-directory here])
                    (git #"submodule" #"status" #"src")))))])
    (λ () (force pr))))

(define (system*/check cmd . args)
  (define code
    (apply system*/exit-code #:set-pwd? #t cmd args))
  (unless (= 0 code)
    (error who "system*/check failed; TODO write a better message")))

(define-values [cmake ctest git]
  (let* ([cmake (lazy (find-executable-path "cmake"))]
         [ctest (lazy (find-executable-path "ctest"))]
         [git (lazy (find-executable-path "git"))]
         [wrap-required
          (λ (it)
            (lazy (let ([cmake (force cmake)]
                        [ctest (force ctest)]
                        [git (force git)])
                    (unless (and cmake ctest)
                      (error who 
                             "required executable~s not found\n  cmake: ~e\n  ctest: ~e\n  git: ~e"
                             (if (= 1 (for/sum ([x (in-list (list cmake ctest git))]
                                                #:unless x)
                                        1))
                                 ""
                                 "s")
                             cmake
                             ctest
                             git))
                    it)))]
         [wrap-proc (λ (it)
                      (λ args (apply system*/check (force it) args)))]
         [wrap (λ (it) (wrap-proc (wrap-required it)))])
    (values (wrap cmake)
            (wrap ctest)
            (wrap git))))

(define (make-directory** dir)
  (make-directory* dir)
  dir)


(define (get-platform)
  (match (assf matching-platform? supported-platforms)
    [(list platform)
     platform]
    [#f
     (error who
            "not running on a supported host platform\n  current platform: ~e"
            (system-library-subpath #f))]))

(define (make [platform (get-platform)]
              #:force? [force? #f]
              #:lib? [lib? #t]
              #:info? [info? #t])
  (define pkg-dir
    (make-directory**
     (build-path here (string-append "libgit2-" platform))))
  (define build-dir
    (make-directory**
     (build-path build-base-dir platform)))
  (define build-dir-so
    (build-path build-dir so-name))
  (define pkg-dir-so
    (build-path pkg-dir so-name))
  ;; make the native library
  (when lib?
    (define must-compile?
      (or force?
          (not (and (file-exists? .src-status)
                    (file-exists? build-dir-so)
                    (file-exists? pkg-dir-so)))
          (not (equal? (file->string .src-status) (get-current-src-status)))
          (not (<= (file-or-directory-modify-seconds build-dir-so)
                   (file-or-directory-modify-seconds pkg-dir-so)))))
    (when must-compile?
      (parameterize ([current-directory build-dir]
                     [current-input-port (open-input-bytes #"")])
        (cmake src)
        (cmake #"--build" build-dir)
        (ctest #"-V"))
      (call-with-output-file* .src-status
        #:exists 'truncate/replace
        (λ (out) (write-string (get-current-src-status) out)))
      (copy-file build-dir-so pkg-dir-so 'replace)))
  ;; make the info.rkt file
  (when info?
    (call-with-output-file* (build-path pkg-dir "info.rkt")
      #:exists 'truncate/replace
      (λ (out)
        (define pkg-desc
          (format "native libraries for \"libgit2\" on \"~a\""
                  platform))
        (write-string "#lang info\n\n" out)
        (for ([sexp (in-list
                     `((define collection "libgit2")
                       (define pkg-desc ,pkg-desc)
                       (define version "0.0")
                       (define pkg-authors '(philip))
                       #\newline
                       (define install-platform ,platform)
                       (define copy-foreign-libs '(,so-name))
                       #\newline
                       (define deps '("base"))))])
          (if (eq? #\newline sexp)
              (newline out)
              (pretty-write sexp out)))
        (newline out)))))