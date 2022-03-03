#lang racket
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

#;
(define git-describe.txt/lazy-string
  (delay/sync
   (string->immutable-string
    (with-output-to-string
      (λ ()
        (parameterize ([current-directory here])
          (invoke git "describe" "--dirty" "--long" "--always")))))))

#;
(with-output-to-file (build-path (built-before-apple/provenance/)
                                 "guix-channels.scm")
  #:permissions read-only-permissions
  (λ ()
    (printf ";; -*- mode: scheme; -*-\n")
    (invoke guix "describe" "-f" "channels")))

;; re tests:
#;
(define-public libgit2-for-racket
  (package
    (inherit libgit2)
    (version %libgit2-version)
    (source libgit2-origin)
    (inputs `())
    (propagated-inputs `())
    (arguments
     (substitute-keyword-arguments (package-arguments libgit2)
       ((#:configure-flags std-cfg-flags '())
        `(append
          ',(target->system-flags (%current-target-system))
          ',%common-configure-flags
          (let ((flags ,std-cfg-flags))
            (filter (lambda (s)
                      (string-prefix? "-DPKG_CONFIG_EXECUTABLE=" s))
                    flags))))
       ((#:tests? _ #f)
        ;; error trying to write
        #f)
       #;
       ((#:phases usual-phases)
       `(modify-phases ,usual-phases
       (add-before 'configure 'mut-src
       (lambda _
       (copy-recursively "." "../mut-srcdir")
       (chdir "../mut-srcdir")))
       #;
       (add-before 'test 'set-clar-tmp
       (lambda _
       (mkdir-p "my-clar-tmp")
       (setenv "CLAR_TMP" "my-clar-tmp")))))))))
