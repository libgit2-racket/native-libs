#lang racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(require "base.rkt"
         racket/cmdline
         racket/match
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
      (define (continue-when-all)
        (when (eq? action 'all)
          (failure-cont)))
      (match action
        [(or 'init 'all)
         (init)
         (continue-when-all)]
        [(or 'apple-run-remote 'all)
         (apple-run-remote)
         (continue-when-all)]
        #;
        [(or 'pack 'all)
         (pack)]
        ;; end of 'all
        ;; -------
        #;['????
           ])])
   null))

