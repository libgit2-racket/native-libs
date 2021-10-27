#lang racket
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(require racket/runtime-path
         (only-in racket/base
                  [string-append-immutable ++])
         "guix/libgit2-for-racket/common.scm"
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

#;
(module+ main
  (for-each build-target-here
            platforms))

(define-runtime-path guix-load-dir "guix/")
(define here (build-path guix-load-dir 'up))
(define default-workspace (build-path here "workspace"))
(define current-workspace
  (make-parameter default-workspace
                  path->complete-path))

(define guix (find-executable-path "guix"))


(struct target (arch os triplet string)
  #:prefab)
(define-syntax-parse-rule (define-targets platforms
                            ;; #f -> host platform
                            [arch:id os:id (~or* triplet:str (~and triplet #f))] ...)
  #:with (platform-string ...) (map (λ (arch os)
                                      (format "~a-~a" (syntax-e arch) (syntax-e os)))
                                    (attribute arch)
                                    (attribute os))
  (define platforms
    '(#s(target arch os triplet platform-string) ...)))
(define-targets platforms
  [x86_64  linux  #f]
  [x86_64  win32  "x86_64-w64-mingw32"]
  [i386    win32  "i686-w64-mingw32"]
  [x86_64  macosx #f]
  [aarch64 macosx "aarch64-darwin"])

(define (apple-os? sym)
  (eq? sym 'macosx))
(define (windows-os? sym)
  (eq? sym 'win32))

(define os->lib-filename
  (match-lambda
    ['win32
     (++ "libgit2-" %so-version ".dll")]
    ['macosx
     (++ "libgit2." %so-version ".dylib")]
    [_
     (++ "libgit2.so." %so-version)]))
(define (os->built-lib-path os)
  (if (windows-os? os)
      (build-path "bin" "libgit2.dll")
      (build-path "lib" (os->lib-filename os))))

(define no-in (open-input-bytes #""))
(define (invoke . args)
  (let ([args (filter values (flatten args))])
    (fprintf (current-error-port) ";; invoking ...\n")
    (pretty-print args (current-error-port))
    (match (parameterize ([current-input-port no-in])
             (apply system*/exit-code args))
      [0
       (void)]
      [code
       (error 'invoke "command ~e failed (code ~e)" (car args) code)])))
(define (list-when test body0 body ...)
  (cond
    [test body0 body ...]
    [else null]))

(define guix-build
  (curry invoke guix "build" "-L" guix-load-dir "--keep-failed"))

(define shared-license-data-dir
  (delay/sync
   (string->path
    (string-trim
     (with-output-to-string
       (λ () (guix-build "libgit2-shared-license-data")))))))

(define guix-channels.scm
  (delay/sync
   (++ ";; -*- mode: scheme; -*-\n"
       (with-output-to-string
         (λ () (invoke guix "describe" "-f" "channels"))))))

(define read-only-permissions
  (bitwise-ior user-read-bit
               group-read-bit
               other-read-bit))

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

(define (call-with-tmp-root-path proc)
  (define tmp-root (build-path (current-workspace) "tmp-store-root"))
  (define (reclaim-tmp-root)
    (match (file-or-directory-type tmp-root #f)
      ['link
       (delete-file tmp-root)]
      [#f
       (void)]
      [other-type
       (raise-arguments-error 'reclaim-tmp-root
                              "root existed and was not a symlink"
                              "path" tmp-root
                              "type" other-type)]))
  (dynamic-wind reclaim-tmp-root
                (λ ()
                  (call-with-continuation-barrier
                   (λ () (proc tmp-root))))
                reclaim-tmp-root))
(define-syntax-parse-rule (let/tmp-root tmp-root:id body:expr ...+)
  (call-with-tmp-root-path (λ (tmp-root) body ...)))
(define (build-target-here a-target)
  (let/tmp-root
   tmp-root
   (match-define (target arch os triplet arch-os) a-target)
   (define dest-dir (build-path (current-workspace) arch-os)) ;; <-------- consider
   (delete-directory/files dest-dir #:must-exist? #f)
   (make-directory* dest-dir)
   (define filename (os->lib-filename os))
   (define dest-pth (build-path dest-dir filename))

   (guix-build "-e"
               "(@ (libgit2-for-racket) libgit2-for-racket)"
               "-r" tmp-root
               (and triplet (++ "--target=" triplet)))

   (copy-file (build-path tmp-root (os->built-lib-path os))
              dest-pth)

   (when (eq? 'linux os)
     (define mode (file-or-directory-permissions dest-pth 'bits))
     (file-or-directory-permissions dest-pth (bitwise-ior mode user-write-bit))
     (invoke guix "shell" "--pure" "--container" "patchelf"
             "--"
             "patchelf" "--set-rpath" "$ORIGIN" dest-pth)
     (file-or-directory-permissions dest-pth mode))))
