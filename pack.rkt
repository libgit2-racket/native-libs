#lang racket
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)



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
