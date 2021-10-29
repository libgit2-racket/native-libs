#lang racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(require "base.rkt"
         racket/cmdline
         racket/match
         (for-syntax racket/base)
         syntax/parse/define
         "init.rkt"
         "apple-run-remote.rkt"
         "pack.rkt")

(define (program-name)
  (define program (find-system-path 'run-file))
  ;; lifted from racket/cmdline
  (string->symbol (if (path? program)
                      (let-values ([(base name dir?) (split-path program)])
                        (if (path? name)
                            (path-element->string name)
                            (path->string program)))
                      program)))

(define-syntax-parse-rule (match/continue val:expr clause ...)
  #:with orig-datum this-syntax
  (let ([the-val val])
    (match/derived the-val orig-datum clause [_ (void)])
    ...))

(module+ main
  (command-line
   #:usage-help
   "x86_64-linux"
   #:help-labels "Actions: (must specify exactly one)"
   #:once-any
   [("--init")
    => (λ (__) 'init)
    '("...")]
   [("--apple-run-remote")
    => (λ (__) 'apple-run-remote)
    '("...")]
   [("--pack")
    => (λ (__) 'pack)
    '("...")]
   [("--all")
    => (λ (__) 'all)
    '("...")]
   #;
   [()
    => (λ () )
    '("TODO sync or something")]
   #:help-labels
   ""
   "Configuration options:"
   #:once-each
   [("--workspace") dir "..."
                    (current-workspace dir)]
   #:once-any
   [("--ssh-file") host+path-file ("..."
                                   "example.com:.")
                   (current-apple-ssh-info
                    (path->complete-path host+path-file))]
   [("--ssh") host+path "..."
              (current-apple-ssh-info host+path)]
   #:help-labels ;; -- and --help
   ""
   "Meta options:"
   #:handlers
   (match-lambda
     ['()
      (error (program-name) "missing required action flag")]
     [_
      #:when (not (and (eq? 'x86_64 (system-type 'arch))
                       (eq? 'linux (system-type 'os*))))
      (raise-arguments-error (program-name)
                             "unsupported host platform;\n expected x86_64-linux"
                             "arch" (system-type 'arch)
                             "os*" (system-type 'os*))]
     [(list action)
      (match/continue
       action
       [(or 'init 'all)
        (init)]
       [(or 'apple-run-remote 'all)
        (apple-run-remote)]
       #;
       [(or 'pack 'all)
        (pack)]
       ;; end of 'all
       ;; -------
       #;['????
          ])])
   null))

