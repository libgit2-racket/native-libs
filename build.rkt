#lang racket
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(define so-version "1.3.0")

(require racket/runtime-path)

(define platforms
  '((x86_64 linux)
    (x86_64 win32)
    (i386 win32)))


(define-runtime-path deps.scm "deps.scm")
(define here (path-only deps.scm))
(define workspace (build-path here "workspace"))

(define guix
  (find-executable-path "guix"))

(define (-D k v)
  (if (path? v)
      (bytes-append #"-D" (string->bytes/locale k) #"=" (path->bytes v))
      (string-append-immutable "-D" k "=" v)))

(define common-flags
  (list (-D "ENABLE_REPRODUCIBLE_BUILDS" "ON")
        (-D "CMAKE_BUILD_TYPE" "Release")
        (-D "REGEX_BACKEND" "builtin")
        (-D "USE_HTTP_PARSER" "builtin")
        (-D "USE_BUNDLED_ZLIB" "ON")
        (-D "USE_NTLMCLIENT" "OFF")
        (-D "USE_SSH" "OFF")
        #;(-D "DEPRECATE_HARD" "ON")))
(define linux-flags
  (list* (-D "USE_HTTPS" "OpenSSL-Dynamic") ;; hopefully will find natipkg or recent-enough system lib
         common-flags))

(define no-in (open-input-bytes #""))
(define (invoke . args)
  (let ([args (flatten args)])
    (printf ";; invoking ...\n")
    (pretty-print args)
    (match (parameterize ([current-input-port no-in])
             (apply system*/exit-code args))
      [0
       (void)]
      [code
       (error 'invoke "command ~e failed (code ~e)" (car args) code)])))

(parameterize ([current-directory here])
  (for ([platform (in-list platforms)]
        #:when (memq (cadr platform) '(linux)))
    (match-define (list arch os) platform)
    (define target (format "~a-~a" arch os))
    (define target-dir (build-path workspace target))
    (define-syntax-rule (define-dir name elem0 elem ...)
      (begin (define name (build-path target-dir elem0 elem ...))
             (make-directory* name)))
    (define-dir build-dir "build")
    (define-dir prefix-dir "prefix-other")
    (define-dir lib-dir "lib")
    (define guix-env
      (curry invoke guix "environment" "--pure" "--container" "-l" deps.scm))
    (guix-env "--"
              "cmake"
              linux-flags
              (-D "CMAKE_INSTALL_PREFIX" prefix-dir)
              (-D "CMAKE_INSTALL_LIBDIR" lib-dir)
              "-S" "src"
              "-B" build-dir)
    (guix-env "--" "cmake" "--build" build-dir)
    (when (equal? platform '(x86_64 linux))
      (guix-env "--ad-hoc" "openssl"
                "--"
                "bash"
                "-c"
                (bytes-append #"LD_LIBRARY_PATH=$LIBRARY_PATH "
                              (path->bytes (build-path build-dir "libgit2_clar")))))
    (guix-env "--" "cmake" "--build" build-dir "--target" "install")))


;; raco cross --workspace workspace/x86_64-linux/racket/ --target x86_64-linux

;; guix environment --pure --container --ad-hoc gcc-toolchain cmake python

;; -DCMAKE_BUILD_TYPE=Release ;?
;; -DREGEX_BACKEND=builtin ; or stub via Racket ?
;; -DUSE_HTTP_PARSER=builtin
;; USE_BUNDLED_ZLIB=ON
;; USE_NTLMCLIENT=OFF
;DEPRECATE_HARD
;USE_SSH=OFF
;USE_HTTPS=OpenSSL-Dynamic
;ENABLE_REPRODUCIBLE_BUILDS=ON