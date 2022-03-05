#lang racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(module+ main
  (command-line
   #:once-any
   [("--meta-pkg")
    "generate for the meta-package"
    (match-define (H-T pkg-version
                       breaking-change-label
                       platforms)
      (json-args))
    (write-info-rkt
     (meta-pkg-body #:pkg-version pkg-version
                    #:breaking-change-label breaking-change-label
                    #:platforms platforms))]
   [("--platform-pkg")
    "generate for platform-specific package"
    (match-define (H-T arch+os
                       pkg-version
                       breaking-change-label
                       lib-filename)
      (json-args))
    (write-info-rkt
     (platform-pkg-body #:arch+os arch+os
                        #:pkg-version pkg-version
                        #:breaking-change-label breaking-change-label
                        #:lib-filename lib-filename))]))

(module+ test
  (write-info-rkt
   (platform-pkg-body #:arch+os "x86_64-linux"
                      #:pkg-version "0.0"
                      #:breaking-change-label ""
                      #:lib-filename "my-lib.so.x.y.z"))
  (for ([i 60])
    (write-char #\; (current-error-port)))
  (newline (current-error-port))
  (write-info-rkt
   (meta-pkg-body #:pkg-version "0.0"
                  #:breaking-change-label ""
                  #:platforms '("i386-win32" "x86_64-linux"))))

(require racket/match
         racket/pretty
         racket/cmdline
         racket/string
         "utils.rkt"
         syntax/to-string
         (for-syntax racket/base
                     racket/syntax-srcloc)
         syntax/parse/define)

(define-syntax-parse-rule (Q v:expr)
  #:with ret (let* ([loc (syntax-srcloc this-syntax)]
                    [col (srcloc-column loc)]
                    [loc* (struct-copy
                           srcloc loc
                           ;; hack to offset for "#,"
                           [column (and col (max 0 (- col 2)))])])
               #`(let ([v-val v]
                       [loc* #,loc*])
                   (if (procedure? v-val)
                       (v-val loc*)
                       (datum->syntax #f v loc*))))
  ret)

(define (platform-pkg-body #:arch+os arch+os
                           #:pkg-version pkg-version
                           #:breaking-change-label breaking-change-label
                           #:lib-filename lib-filename)
  #`{(define pkg-name #,(Q (make-pkg-name breaking-change-label arch+os)))
     (define collection "libgit2") ;; copy-foreign-libs requires a collection
     ;;
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

(define (meta-pkg-body #:pkg-version pkg-version
                       #:breaking-change-label breaking-change-label
                       #:platforms platforms)
  (define (->pkg-name suffix)
    (make-pkg-name breaking-change-label suffix))
  (define (column+ loc n)
    (struct-copy srcloc loc
                 [column (+ (srcloc-column loc) n)]))
  (define (with-line loc line)
    (struct-copy srcloc loc [line line]))
  (define (->stx v loc)
    (datum->syntax #f v loc))
  (define (make-deps loc)
    (let* ([loc (column+ loc 1)]; offset for open paren
           [line0 (srcloc-line loc)])
      (cons
       (quasisyntax/loc loc
         "base")
       (for/list ([arch+os (in-list (sort platforms arch+os<?))]
                  [this-line (in-range (add1 line0) +inf.0 2)])
         (let* ([loc (with-line loc this-line)]
                [name-loc (column+ loc 1)] ;; open bracket
                [name-str (->pkg-name arch+os)]
                [kw-loc (column+ name-loc (+ 3 ;; space and "
                                             (string-length name-str)))])
           (define (->post-kw-loc kw)
             (column+ kw-loc (+ 3 (string-length (keyword->string kw)))))
           (with-syntax ([~name
                          (->stx name-str name-loc)]
                         [~#:platform
                          (->stx '#:platform kw-loc)]
                         [~platform
                          (->stx (arch+os->platform-spec arch+os)
                                 (->post-kw-loc '#:platform))]
                         [~#:version
                          (->stx '#:version (with-line kw-loc (add1 this-line)))]
                         [~pkg-version
                          (syntax/loc
                              (with-line (->post-kw-loc '#:version) (add1 this-line))
                            ,version)])
             (syntax/loc loc
               [~name ~#:platform ~platform
                      ~#:version ~pkg-version])))))))
  #`{(define pkg-name #,(Q (->pkg-name "native-libs")))
     (define collection 'multi)
     ;;
     (define version #,(Q pkg-version))
     (define pkg-desc #,(Q "native libraries for \"libgit2\" (meta-package)"))
     (define pkg-authors '(philip))
     ;;
     (define license
       '(Apache-2.0 OR MIT))
     ;;
     (define deps
       `#,(Q make-deps))})

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

(define (arch+os<? a b)
  (define split
    (match-lambda
      [(pregexp #px"^([^-]+)-([^-]+)$" (list _ arch os))
       (values arch os)]))
  (let-values ([{a-arch a-os} (split a)]
               [{b-arch b-os} (split b)])
    (or (string<? a-os b-os)
        (and (string=? a-os b-os)
             (string<? a-arch b-arch)))))
