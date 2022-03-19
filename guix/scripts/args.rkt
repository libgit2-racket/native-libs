#lang racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(provide RKT_NOT_A_DRILL
         match-sexpr-args
         H-T
         ~str
         ++
         ;; from RKT_JSON_ARGS
         pkg-version
         breaking-change-label
         so-version
         deprecate-hard
         cfg-flags-common
         cfg-flags-unix
         cfg-flags-windows
         libgit2-version
         libgit2-url
         libgit2-rev
         libgit2-sha256
         self.rev ;; (or/c string? #f)
         self.lastModifiedDate
         self.narHash
         nixpkgs.rev ;; string?
         nixpkgs.lastModifiedDate
         nixpkgs.narHash
         nixpkgs.owner
         nixpkgs.repo
         nixpkgs.ref)

(require racket/match
         racket/port
         json
         syntax/parse/define
         (for-syntax racket/base
                     (only-in syntax/parse
                              [attribute $$])
                     racket/syntax)
         (rename-in racket/base
                    [string-append-immutable ++]))


(define-syntax-parse-rule (~str x:id)
  #:with s (datum->syntax #'x (symbol->string (syntax->datum #'x)))
  s)

(define-syntax-parse-rule (define-env var:id ...+)
  (begin (define var (getenv (~str var))) ...))

(define-syntax-parse-rule (match-env VAR:id parse-from-string:expr pat:expr default:expr)
  (match-define pat
    (cond
      [VAR
       => parse-from-string]
      [(not RKT_NOT_A_DRILL)
       default]
      [else
       (error 'match-env (~str VAR))])))

(define-env RKT_NOT_A_DRILL RKT_JSON_ARGS RKT_SEXPR_ARGS)

(define-syntax-parse-rule (match-sexpr-args pat:expr default:expr)
  (match-env RKT_SEXPR_ARGS string->sexpr pat default))

(define (string->sexpr s)
  (call-with-input-string s read))

(define beautify-lastModifiedDate
  (match-lambda
    [(pregexp #px"^(\\d{4})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})$"
              (list _ yyyy mm dd HH MM SS))
     (++ yyyy "-" mm "-" dd "T" HH ":" MM ":" SS "Z")]))

(define-match-expander H-T
  (let ()
    (define-syntax-class key-id
      #:opaque
      (pattern :id))
    (define-syntax-class match-pattern
      #:description "match pattern"
      #:opaque
      (pattern :expr))
    (define (->prefix id prefix)
      (if prefix
          (format-id id "~a~a" prefix id #:subs? #t)
          id))
    (define-syntax-class (mandatory-pat prefix)
      #:description "mandatory key pattern"
      #:attributes {lhs rhs}
      (pattern var:key-id
               #:with [lhs rhs] #`['var #,(->prefix #'var prefix)])
      (pattern [#:_ var:key-id rhs:match-pattern]
               #:with lhs #''var)
      (pattern ((~datum and) var:key-id pat:match-pattern ...+)
               #:with [lhs rhs] #`['var (and #,(->prefix #'var prefix) pat ...)]))
    (syntax-parser
      [(_ (~optional (~seq #:prefix prefix:id))
          (~alt (~describe "optional key pattern"
                           [#:? opt-var:key-id
                            (~describe #:role "failure-result" #:opaque "expression"
                                       default:expr)
                            (~optional pat:match-pattern)])
                (~var || (mandatory-pat (attribute prefix))))
          ...)
       #:with (prefixed-opt-var ...) (for/list ([id (attribute opt-var)])
                                       (->prefix id (attribute prefix)))
       #'(and (hash-table
               [lhs rhs] ...
               [_ _] (... ...))
              (app (λ (hsh) (hash-ref hsh 'opt-var default))
                   (~? (and prefixed-opt-var pat) prefixed-opt-var))
              ...)])))

(match-env RKT_JSON_ARGS
           string->jsexpr
           (H-T pkg-version
                breaking-change-label
                so-version
                deprecate-hard
                cfg-flags-common
                cfg-flags-unix
                cfg-flags-windows
                libgit2-version
                libgit2-url
                libgit2-rev
                libgit2-sha256
                [#:_ self-source-info
                 (H-T #:prefix self.
                      rev ;; (or/c string? #f)
                      [#:_ lastModifiedDate
                       (app beautify-lastModifiedDate
                            self.lastModifiedDate)]
                      narHash)]
                [#:_ nixpkgs-source+lock-info
                 (H-T #:prefix nixpkgs.
                      rev ;; string?
                      [#:_ lastModifiedDate
                       (app beautify-lastModifiedDate
                            nixpkgs.lastModifiedDate)]
                      narHash
                      owner
                      repo
                      ref)])
           #hasheq([pkg-version . "0.0.0.999"]
                   [breaking-change-label . "unstable"]
                   [so-version . "0"]
                   [deprecate-hard . #t]
                   [cfg-flags-common . ()]
                   [cfg-flags-unix . ()]
                   [cfg-flags-windows . ()]
                   [libgit2-version . "0.28.0"]
                   [libgit2-url . "https://github.com/libgit2/libgit2"]
                   [libgit2-rev . "v0.28.0"]
                   [libgit2-sha256 . "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]
                   [self-source-info
                    . #hasheq([rev . #f]
                              [lastModifiedDate . "19181111110000"]
                              [narHash . "sha256-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx="])]
                   [nixpkgs-source+lock-info
                    . #hasheq([lastModifiedDate . "20220302215953"]
                              [narHash . "sha256-mKDoCi5Baqainjf0Nx6oTPhaxKQdj4P17XTdhTtXGRA="]
                              [rev . "ed02c2ba0384b2800db41333045a6fb781f12aac"]
                              [owner . "NixOs"]
                              [repo . "nixpkgs"]
                              [ref . "nixos-21.11"])]))