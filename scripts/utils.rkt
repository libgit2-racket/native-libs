#lang racket/base

(provide make-pkg-name
         json-args
         default-json-args
         beautify-lastModifiedDate
         H-T
         ++)

(require racket/match
         racket/string
         racket/promise
         json
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax)
         (rename-in racket/base
                    [string-append-immutable ++]))

(define (make-pkg-name breaking-change-label suffix)
  (++ "libgit2-"
      (make-branch-name breaking-change-label suffix)))

(define (make-branch-name breaking-change-label suffix)
  (++ breaking-change-label
      (if (non-empty-string? breaking-change-label)
          "-"
          "")
      suffix))

(define env-var-name
  "RKT_JSON_ARGS")

(define json-args
  (let* ([json-args
          (delay/sync
           (match (getenv env-var-name)
             [#f #f]
             [(? string? (app string->jsexpr (? hash? js)))
              js]))]
         [json-args
          (λ ()
            (force json-args))])
    json-args))

(define default-json-args
  #hasheq([pkg-version . "0.0.0.999"]
          [breaking-change-label . "unstable"]
          [platforms . ("i386-win32" "x86_64-linux")]
          [self-source-info
           . #hasheq([lastModifiedDate . "19181111110000"]
                     [narHash . "sha256-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx="])]
          [nixpkgs-source+lock-info
           . #hasheq([lastModifiedDate . "20220302215953"]
                     [narHash . "sha256-mKDoCi5Baqainjf0Nx6oTPhaxKQdj4P17XTdhTtXGRA="]
                     [rev . "ed02c2ba0384b2800db41333045a6fb781f12aac"]
                     ;; .locked
                     [type . "github"]
                     [owner . "NixOs"]
                     [repo . "nixpkgs"]
                     ;; .original
                    
                     [ref . "nixos-21.11"])]
          [libgit2-info
           . #hasheq([version . "0.28.0"]
                     [sha256 . "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]
                     [rev . "v0.28.0"]
                     [owner . "libgit2"]
                     [repo . "libgit2"])]
          [arch+os . "x86_64-linux"]
          [lib-filename . "demo.so.x.y.z"]))

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
