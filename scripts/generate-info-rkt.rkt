#lang racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(module+ main
  (let ([_arch+os #f]
        [_pkg-version #f]
        [_lib-filename #f])
    (command-line
     #:once-each
     [("--arch+os") arch+os "e.g. \"x86_64-win32\""
                    (set! _arch+os arch+os)]
     [("--pkg-version") pkg-version "version of the Racket package"
                        (set! _pkg-version pkg-version)]
     [("--lib-filename") lib-filename "e.g. \"libgit2.z.y.dylib\""
                         (set! _lib-filename lib-filename)]
     #:args ()
     (let ([arch+os _arch+os]
           [pkg-version _pkg-version]
           [lib-filename _lib-filename])
       (cond
         [(or arch+os pkg-version lib-filename)
          (define-syntax-rule (assert-flag id)
            (unless id
              (error who "missing required argument \"--~a\"" 'id)))
          (assert-flag arch+os)
          (assert-flag pkg-version)
          (assert-flag lib-filename)
          (generate-info.rkt #:arch+os arch+os
                             #:pkg-version pkg-version
                             #:lib-filename lib-filename)]
         [else
          (eprintf "~a: no arguments given; doing nothing\n" who)])))))

(require racket/match
         racket/pretty
         racket/cmdline
         (rename-in racket/base
                    [string-append-immutable ++]))

(define who 'generate-info.rkt)

(define (generate-info.rkt #:arch+os arch+os
                           #:pkg-version pkg-version
                           #:lib-filename lib-filename)
  (call-with-values (λ ()
                      (make-info.rkt-spec #:arch+os arch+os
                                          #:pkg-version pkg-version
                                          #:lib-filename lib-filename))
                    write-info.rkt-spec))
(define (make-info.rkt-spec #:arch+os arch+os
                            #:pkg-version pkg-version
                            #:lib-filename lib-filename)
  (define racket-portion-license '(Apache-2.0 OR MIT))
  (values
   (format "#lang info\n;; SPDX-License-Identifier: ~s\n\n"
           racket-portion-license)
   `((define pkg-name ,(++ "libgit2-" arch+os))
     (define collection "libgit2")
     (define version ,pkg-version)
     (define pkg-desc ,(++ "native libraries for \"libgit2\" on \"" arch+os "\""))
     (define pkg-authors '(philip))
     #\newline
     (define install-platform ,(match arch+os
                                 ["x86_64-linux"
                                  #rx"^x86_64-linux(?:-natipkg)?$"]
                                 ["x86_64-win32"
                                  "win32\\x86_64"]
                                 ["i386-win32"
                                  "win32\\i386"]
                                 [_
                                  arch+os]))
     (define copy-foreign-libs '(,lib-filename))
     #\newline
     (define license
       '((GPL-2.0-only WITH GCC-exception-2.0)
         AND
         ,racket-portion-license))
     #\newline
     (define deps '("base")))))

(define (write-info.rkt-spec prelude body)
  (write-string prelude)
  (for-each (match-lambda
              [#\newline
               (newline)]
              [v
               (pretty-write v)])
            body))
