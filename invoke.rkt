#lang racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

;; This file is shared with the x86_64-linux scripts.
(require racket/contract
         racket/list
         racket/pretty
         racket/match
         racket/system)

(provide (contract-out
          [invoke (-> (flat-rec-contract
                       arg-jumble/c
                       path?
                       string-no-nuls?
                       bytes-no-nuls?
                       #f
                       '()
                       (cons/c arg-jumble/c arg-jumble/c))
                      ...
                      any)]))

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

