#lang racket
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(require racket/runtime-path
         (only-in racket/base
                  [string-append-immutable ++])
         "guix/libgit2-for-racket/common.scm"
         "apple-nix-skel/invoke.rkt"
         syntax/parse/define
         racket/symbol)

;; install_name_tool -id libgit2.1.3.dylib libgit2.1.3.dylib
#|
hart:apple-nix-bundle philip$ objdump -macho --dylibs-used libgit2.1.3.dylib 
libgit2.1.3.dylib:
        libgit2.1.3.dylib (compatibility version 1.3.0, current version 1.3.0)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1238.60.2)
        /nix/store/wa0kxp38vna4kig3s0m7wx7klq4rcsmx-libiconv-50/lib/libiconv.dylib (compatibility version 7.0.0, current version 7.0.0)
hart:apple-nix-bundle philip$ install_name_tool -change /nix/store/wa0kxp38vna4kig3s0m7wx7klq4rcsmx-libiconv-50/lib/libiconv.dylib /usr/lib/libiconv.2.dylib libgit2.1.3.dylib 
hart:apple-nix-bundle philip$ objdump -macho --dylibs-used libgit2.1.3.dylib libgit2.1.3.dylib:
        libgit2.1.3.dylib (compatibility version 1.3.0, current version 1.3.0)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1238.60.2)
        /usr/lib/libiconv.2.dylib (compatibility version 7.0.0, current version 7.0.0)
|#

(define-runtime-path guix-load-dir "guix/")
(define here (build-path guix-load-dir 'up))
(define default-workspace (build-path here "workspace"))
(define current-workspace
  (make-parameter default-workspace
                  path->complete-path))

(define guix (find-executable-path "guix"))

(define-match-expander target
  (syntax-parser
    [(_ arch os triplet string)
     #'(list arch os triplet string)]))

(define (list-when test body0 body ...)
  (cond
    [test body0 body ...]
    [else null]))

(define guix-build
  (curry invoke guix "build" "-L" guix-load-dir "--keep-failed"))

(define (build-shared-license-data)
  (guix-build "libgit2-shared-license-data"
              "-r" (build-path (built-before-apple-dir) "shared-license-data")))

(define guix-channels.scm
  (delay/sync
   (++ ";; -*- mode: scheme; -*-\n"
       (with-output-to-string
         (λ () (invoke guix "describe" "-f" "channels"))))))

(define read-only-permissions
  (bitwise-ior user-read-bit
               group-read-bit
               other-read-bit))
#;
(define (add-pkg-info a-target dest-dir)
  (match-define (target arch os triplet arch-os) a-target)
  (define platform-spec
    (match arch-os
      ["x86_64-linux"
       #rx"^x86_64-linux(?:-natipkg)?$"]
      ["x86_64-win32"
       "win32\\x86_64"]
      ["i386-win32"
       "win32\\i386"]
      [_
       platform-spec]))
  (define racket-portion-license '(Apache-2.0 OR MIT))
  (define info
    `((define pkg-name ,(++ "libgit2-" arch-os))
      (define collection "libgit2")
      (define version ,%racket-pkg-version)
      (define pkg-desc ,(++ "native libraries for \"libgit2\" on \"" arch-os "\""))
      (define pkg-authors '(philip))
      #\newline
      (define install-platform ,platform-spec)
      (define copy-foreign-libs '(,(os->lib-filename os)))
      #\newline
      (define license
        '((GPL-2.0-only WITH GCC-exception-2.0)
          AND
          ,racket-portion-license))
      #\newline
      (define deps '("base"))))
  (call-with-output-file* (build-path dest-dir "info.rkt")
                          #:exists 'truncate/replace
                          #:permissions read-only-permissions
                          (λ (out)
                            (fprintf out
                                     "#lang info\n;; SPDX-License-Identifier: ~s\n\n"
                                     racket-portion-license)
                            (for-each (match-lambda
                                        [#\newline
                                         (newline out)]
                                        [v
                                         (pretty-write v out)])
                                      info)))
  ;; TODO: generate a README.md
  (parameterize ([current-directory (force shared-license-data-dir)])
    (for ([pth (in-directory)])
      (copy-directory/files pth (build-path dest-dir pth)))))


(define (built-before-apple-dir)
  (build-path (current-workspace) "built-before-apple"))


(define (build-non-apple-target a-target)
  (match-define (target arch os triplet arch-os) a-target)
  (define dest-dir (build-path (built-before-apple-dir) "pkgs" arch-os))
  (delete-directory/files dest-dir #:must-exist? #f)
  (make-directory* dest-dir)
  (define filename (os->lib-filename os))
  (define dest-pth (build-path dest-dir filename))

  (let ([tmp-gc-root-dir
         (make-temporary-file "tmp-gc-root-dir~a" 'directory)])
    (dynamic-wind
     void
     (λ ()
       (define store-dir
         (second
          (string-split
           (with-output-to-string
             (λ ()
               (guix-build "-e"
                           "(@ (libgit2-for-racket) libgit2-for-racket)"
                           "-r" (build-path tmp-gc-root-dir "built") ;; built-0 and -1
                           (and triplet (++ "--target=" triplet))))))))
       (copy-file (build-path tmp-gc-root-dir (os->built-lib-path os))
                  dest-pth))
     (λ ()
       (delete-directory/files tmp-gc-root-dir))))

  (when (eq? 'linux os)
    (define mode (file-or-directory-permissions dest-pth 'bits))
    (file-or-directory-permissions dest-pth (bitwise-ior mode user-write-bit))
    (invoke guix "shell" "--pure" "--container" "patchelf"
            "--"
            "patchelf" "--set-rpath" "$ORIGIN" dest-pth)
    (file-or-directory-permissions dest-pth mode)))
