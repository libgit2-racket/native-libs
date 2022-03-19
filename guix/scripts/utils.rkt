#lang racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(provide make-pkg-name
         make-branch-name
         arch+os<?
         ++)

(require racket/match
         racket/string
         "args.rkt"
         (for-syntax))

(define (make-pkg-name breaking-change-label suffix)
  (++ "libgit2-"
      (make-branch-name breaking-change-label suffix)))

(define (make-branch-name breaking-change-label suffix)
  (++ breaking-change-label
      (if (non-empty-string? breaking-change-label)
          "-"
          "")
      suffix))

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
