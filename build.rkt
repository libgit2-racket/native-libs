#lang racket
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(define so-version "1.3")
(define pkg-version "0.0")

(require racket/runtime-path)

(define platforms
  '(#;(x86_64 linux)
    (x86_64 win32)
    #;(i386 win32)))


(define-runtime-path deps.scm "deps.scm")
(define-runtime-path manifest-mingw.scm "manifest-mingw.scm")
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
        (-D "DEPRECATE_HARD" "ON")))
(define linux-flags
  (list* (-D "USE_HTTPS" "OpenSSL-Dynamic") ;; hopefully will find natipkg or recent-enough system lib
         common-flags))
(define windows-flags
  (list* common-flags))

(define platform->target-toolchain
  (match-lambda
    ['(x86_64 win32)
     "x86_64-w64-mingw32"]
    ['(i386 win32)
     "i686-w64-mingw32"]
    [_
     #f]))

(define (environment-variables-for-platform platform)
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set!
   env
   #"RKT_LIBGIT2_TARGET"
   (string->bytes/locale (platform->target-toolchain platform)))
  env)

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

;; cmake -DCMAKE_SYSTEM_NAME=Windows -DCMAKE_C_COMPILER=x86_64-w64-mingw32-gcc -DDLLTOOL=x86_64-w64-mingw32-dlltool ../src/
;; ^ this works, sans-Guix

(for ([platform (in-list platforms)]
      #:when (memq (cadr platform) '(linux win32)))
  (parameterize ([current-directory here]
                 [current-environment-variables (environment-variables-for-platform platform)])
    (match-define (list arch os) platform)
    (define linux? (eq? 'linux os))
    (define host? (equal? platform '(x86_64 linux)))
    (define toolchain (platform->target-toolchain platform))
    (define target (format "~a-~a" arch os))
    (define target-dir (build-path workspace target))
    (define-syntax-rule (define-dir name elem0 elem ...)
      (begin (define name (build-path target-dir elem0 elem ...))
             (make-directory* name)))
    (define-dir build-dir "build")
    (define-dir prefix-dir "prefix-other")
    (define-dir lib-dir "lib")
    (define-dir pkg-dir "pkg")
    (define-values [configure-flags manifest-flags]
      (if linux?
          (values linux-flags `("-l" ,deps.scm))
          (values windows-flags `("-m" ,manifest-mingw.scm))))
    (define cross-config-flags
      (if host?
          null
          (list (-D "CMAKE_SYSTEM_NAME" (if linux? "Linux" "Windows"))
                ;(-D "CMAKE_LIBRARY_PATH" "/home/philip/.guix-profile/lib/")
                (-D "CMAKE_RC_COMPILER" (string-append toolchain "-windres"))
                (-D "DLLTOOL" (string-append toolchain "dlltool"))
                ;(-D "CMAKE_C_COMPILER_ID" "GNU")
                ;(-D "CMAKE_C_IMPLICIT_INCLUDE_DIRECTORIES" "/home/philip/.guix-profile/include")
                (-D "CMAKE_FIND_ROOT_PATH" "/home/philip/.guix-profile")
                (-D "CMAKE_FIND_ROOT_PATH_MODE_INCLUDE" "ONLY")
                (-D "CMAKE_C_COMPILER" (string-append toolchain "-gcc"))
                (-D "CMAKE_CXX_COMPILER" (string-append toolchain "-g++")))))
    (define guix-env
      (curry invoke guix "environment" "--pure" "--container" "--link-profile" manifest-flags))
    (guix-env "--"
              "cmake"
              cross-config-flags
              configure-flags
              (-D "CMAKE_INSTALL_PREFIX" prefix-dir)
              (-D "CMAKE_INSTALL_LIBDIR" lib-dir)
              "-S" "src"
              "-B" build-dir)
    (guix-env "--" "cmake" "--build" build-dir)
    (when host?
      (guix-env "--ad-hoc" "openssl"
                "--"
                "bash"
                "-c"
                (bytes-append #"LD_LIBRARY_PATH=$LIBRARY_PATH "
                              (path->bytes (build-path build-dir "libgit2_clar")))))
    (guix-env "--" "cmake" "--build" build-dir "--target" "install")
    (define so-file
      (string-append-immutable "libgit2.so." so-version))
    (copy-file (build-path lib-dir so-file)
               (build-path pkg-dir so-file))
    (invoke guix "environment" "--pure" "--container" "--ad-hoc" "patchelf"
            "--" "patchelf" "--remove-rpath" (build-path pkg-dir so-file))))


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