#lang racket/base

(require setup/matching-platform
         racket/system
         racket/file
         racket/port
         racket/promise
         racket/pretty
         racket/match
         racket/runtime-path)

(define supported-platforms
  ;;   (listof PlatformSpec)
  ;; where a PlatformSpec is:
  ;;   (cons/c string? (or/c (list/c) (list/c (or/c string? regexp?))))
  ;; The first element of a PlatformSpec is the cannonical
  ;;   platform string, in the sense of `matching-platform?`.
  ;; If there is a second element, it is used instead of the first
  ;;   for `install-platform` in the "info.rkt" file (to support natipkg).
  `(["x86_64-linux" #rx"^x86_64-linux(?:-natipkg)?$"]
    ["i386-linux"]
    ["win32\\x86_64"]
    ["win32\\i386"]
    ["x86_64-macosx"]))

(define pkg-version "0.0")

(define cmake-build-type
  ;; -DCMAKE_BUILD_TYPE=RELEASE [default: DEBUG]
  #"DEBUG")

(define version "")

;; to cross-compile i386-linux:
;; sudo apt install libc6-dev-i386
;; sudo apt install libssl-dev ;; could we just use Racket's ???
;; add this to both cmake calls:
;; -DCMAKE_C_FLAGS=-m32
;; cross-compiling on Linux seems to be a problem for ctest, though

;; SSH disabled on Mac, Linux right now
;; iconv disabled on Linux right now ;; this is w/ Racket, right ???
;; appveyor w64 doesn't find SSH or iconv

(module+ main
  (require racket/cmdline)
  (printf "> (system-library-subpath #f)\n  ~v\n" (system-library-subpath #f))
  (for ([p (in-list '("win32\\x86_64" "win32\\i386"))])
    (printf "(matching-platform? ~v)\n  ~v\n" (matching-platform? p)))
  (exit 1)
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
            ['macosx "libgit2~a.dylib"]
            ['windows "git2~a.dll"]
            [_ "libgit2.so~a"])
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
    (error who
           "system*/check failed with exit code ~e\n  arguments: ~e"
           code
           args)))

(define-values [cmake ctest git]
  (let* ([find (λ (s)
                 (let ([s (case (system-type)
                            [(windows) (string-append s ".exe")]
                            [else s])])
                   (lazy (find-executable-path s))))]
         [cmake (find "cmake")]
         [ctest (find "ctest")]
         [git (find "git")])
    (define exn-promise
      (lazy (let ([cmake (force cmake)]
                  [ctest (force ctest)]
                  [git (force git)])
              (unless (and cmake ctest git)
                (error who 
                       "required executable~s not found\n  cmake: ~e\n  ctest: ~e\n  git: ~e"
                       (if (= 1 (for/sum ([x (in-list (list cmake ctest git))]
                                          #:unless x)
                                  1))
                           ""
                           "s")
                       cmake
                       ctest
                       git)))))
    (define ((wrap it) . args)
      (force exn-promise)
      (apply system*/check (force it) args))
    (values (wrap cmake)
            (wrap ctest)
            (wrap git))))

(define (make-directory** dir)
  (make-directory* dir)
  dir)


(define (get-platform-spec)
  (or (for/first ([spec (in-list supported-platforms)]
                  #:when (matching-platform? (car spec)))
        spec)
      (error who
             "not running on a supported host platform\n  current platform: ~e"
             (system-library-subpath #f))))


(define (make #:force? [force? #f]
              #:lib? [lib? #t]
              #:info? [info? #t])
  (apply make* #:force? force? #:lib? lib? #:info? info? (get-platform-spec)))


(define (make* platform
               [install-platform platform]
               #:force? [force? #f]
               #:lib? [lib? #t]
               #:info? [info? #t])
  (define pkg-dir
    (make-directory**
     (build-path
      here
      (string-append "libgit2-"
                     (regexp-replace* #rx"\\\\" platform "-")))))
  (define build-dir
    (make-directory**
     (build-path build-base-dir platform)))
  (define build-dir-so
    (build-path (match (system-type)
                  ['windows
                   (build-path build-dir
                               (string-titlecase
                                (bytes->string/utf-8
                                 cmake-build-type)))]
                  [_
                   build-dir])
                so-name))
  (define pkg-dir-so
    (build-path pkg-dir so-name))
  (define sep
    ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  ;; make the native library
  (when lib?
    ;; compile if needed
    (define must-compile?
      (or force?
          (not (and (file-exists? .src-status)
                    (file-exists? build-dir-so)))
          (not (equal? (file->string .src-status) (get-current-src-status)))))
    (define must-copy?
      (or must-compile?
          (not (file-exists? pkg-dir-so))
          (not (<= (file-or-directory-modify-seconds build-dir-so)
                   (file-or-directory-modify-seconds pkg-dir-so)))))
    (when must-compile?
      (parameterize ([current-directory build-dir]
                     [current-input-port (open-input-bytes #"")])
        (cmake (bytes-append #"-DCMAKE_BUILD_TYPE=" cmake-build-type)
               src)
        (cmake #"--build" build-dir)
        (ctest #"-V"))
      (call-with-output-file* .src-status
        #:exists 'truncate/replace
        (λ (out) (write-string (get-current-src-status) out))))
    (when (or must-compile? must-copy?)
      (printf "~a\n~a\n\n\n~a~a~a\n\n\n~a\n~a\n"
              sep sep
              (if must-compile? (format "~a: build finished" who) "")
              (if (and must-compile? must-copy?) "\n" "")
              (if must-copy?
                  (format "~a: copying\n  from: ~e\n  to: ~e"
                          who build-dir-so pkg-dir-so)
                  "")
              sep sep))
    ;; copy if needed
    (when must-copy?
      (copy-file build-dir-so pkg-dir-so 'replace)))
  ;; make the info.rkt file
  (when info?
    (define info.rkt (build-path pkg-dir "info.rkt"))
    (printf "~a\n~a\n\n\n~a: writing \"info.rkt\"\n  to: ~e\n\n\n~a\n~a\n"
            sep sep who info.rkt sep sep)
    (call-with-output-file* info.rkt
      #:exists 'truncate/replace
      (λ (out)
        (define pkg-desc
          (format "native libraries for \"libgit2\" on \"~a\""
                  platform))
        (write-string "#lang info\n\n" out)
        (for ([sexp (in-list
                     `((define collection "libgit2")
                       (define pkg-desc ,pkg-desc)
                       (define version ,pkg-version)
                       (define pkg-authors '(philip))
                       #\newline
                       (define install-platform ,install-platform)
                       (define copy-foreign-libs '(,so-name))
                       #\newline
                       (define deps '("base"))))])
          (if (eq? #\newline sexp)
              (newline out)
              (pretty-write sexp out)))
        (newline out)))))
