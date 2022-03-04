#lang racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(module+ main
  (command-line
   #:once-any
   [("--platform-pkg")
    "generate for platform-specific package"
    (match (getenv "RKT_JSON_ARGS")
      [(app string->jsexpr
            (hash-table ['arch+os arch+os]
                        ['pkg-version pkg-version]
                        ['breaking-change-label breaking-change-label]
                        ['lib-filename lib-filename]
                        [_ _] ...))
       (write-info-rkt
        (platform-pkg-body #:arch+os arch+os
                           #:pkg-version pkg-version
                           #:breaking-change-label breaking-change-label
                           #:lib-filename lib-filename))])]))

(module+ test
  (write-info-rkt (platform-pkg-body #:arch+os "x86_64-linux"
                                     #:pkg-version "0.0"
                                     #:breaking-change-label ""
                                     #:lib-filename "my-lib.so.x.y.z")))

(require racket/match
         racket/pretty
         racket/cmdline
         racket/string
         json
         syntax/to-string
         (for-syntax racket/base
                     racket/syntax-srcloc)
         syntax/parse/define
         (rename-in racket/base
                    [string-append-immutable ++]))

(define-syntax-parse-rule (Q v:expr)
  #:with ret #`(datum->syntax (quote-syntax v)
                              v
                              #,(let* ([loc (syntax-srcloc this-syntax)]
                                       [col (srcloc-column loc)])
                                  (struct-copy
                                   srcloc loc
                                   ;; hack to offset for "#,"
                                   [column (and col (max 0 (- col 2)))])))
  ret)

(define (platform-pkg-body #:arch+os arch+os
                           #:pkg-version pkg-version
                           #:breaking-change-label breaking-change-label
                           #:lib-filename lib-filename)
  #`{(define pkg-name #,(Q (make-pkg-name breaking-change-label arch+os)))
     (define collection 'multi)
     (define version #,(Q pkg-version))
     (define pkg-desc #,(Q (++ "native libraries for \"libgit2\" on \"" arch+os "\"")))
     (define pkg-authors '(philip))
     ;;
     (define install-platform #,(Q (arch+os->platform-spec arch+os)))
     (define copy-foreign-libs '(#,(Q lib-filename)))
     ;;
     (define license
       '((GPL-2.0-only WITH GCC-exception-2.0)
         AND
         (Apache-2.0 OR MIT)))
     ;;
     (define deps '("base"))})

(define (make-pkg-name breaking-change-label suffix)
  (++ "libgit2-"
      breaking-change-label
      (if (non-empty-string? breaking-change-label)
          "-"
          "")
      suffix))

(define arch+os->platform-spec
  (match-lambda
    ["x86_64-linux"
     #rx"^x86_64-linux(?:-natipkg)?$"]
    ["x86_64-win32"
     "win32\\x86_64"]
    ["i386-win32"
     "win32\\i386"]
    [arch+os
     arch+os]))

(define (write-info-rkt body)
  (printf "#lang info\n;; SPDX-License-Identifier: ~s\n\n"
          '(Apache-2.0 OR MIT))
  (write-string (syntax->string body))
  (newline))
