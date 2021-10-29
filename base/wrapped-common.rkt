#lang racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(require (for-syntax racket/base
                     syntax/parse))

(define-syntax wrap-scheme-module
  (syntax-parser
    [(_ mod-pth)
     #:with (orig-id ...) (let ([phase0
                                 (assoc 0 (syntax-local-module-exports #'mod-pth))])
                            (for/list ([sym (in-list (if phase0
                                                         (cdr phase0)
                                                         null))])
                              (datum->syntax #'mod-pth sym #'mod-pth #'mod-pth)))
     #:with (internal-id ...) ((make-syntax-introducer) #'(orig-id ...))
     #`(begin
         (require (rename-in mod-pth
                             [orig-id internal-id]
                             ...))
         (define orig-id (scheme->racket internal-id))
         ...
         ;
         (provide (all-from-out mod-pth) ;; it seems hygene excludes internal-id ... already
                  orig-id ...))]))

(define (scheme->racket v)
  (cond
    [(mpair? v)
     (cons (scheme->racket (mcar v))
           (scheme->racket (mcdr v)))]
    [(procedure? v)
     (define wrap-result
       (case-lambda
         [(v)
          (scheme->racket v)]
         [v*
          (apply values (map scheme->racket v*))]))
     (impersonate-procedure v
                            (case-lambda
                              [(arg)
                               (values wrap-result arg)]
                              [args
                               (apply values wrap-result args)]))]
    [(string? v)
     (string->immutable-string v)]
    [else
     v]))

(wrap-scheme-module "../guix/libgit2-for-racket/common.scm")

(module+ test
  %all-platforms)
