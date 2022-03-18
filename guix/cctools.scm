(define-module (cctools)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages llvm)
  #:use-module ((guix licenses) #:prefix license:))

(define-public cctools
  (let ((cctools-version "973.0.1")
        (ld64-version "609")
        (revision "0")
        (commit "04663295d0425abfac90a42440a7ec02d7155fea"))
    (package
      (name "cctools")
      (version (git-version (string-append cctools-version
                                           "-ld64-"
                                           ld64-version)
                            revision
                            commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tpoechtrager/cctools-port")
               (commit commit)))
         (sha256
          (base32 "0vihfa8y64vvd3pxy8qh4mhcnzinxh9flpz9dvw4wch4zj2nnfjs"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (native-inputs (list clang-toolchain))
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda args
                (chdir "cctools")))
            (add-after 'chdir 'find-linux-limits-h
              (lambda* (#:key inputs #:allow-other-keys)
                ;; FIXME: This is a very ugly way to make
                ;;     #include <linux/limits.h>
                ;; work---what is a better way?
                (setenv "CPATH"
                        (list->search-path-as-string
                         (cons #$(file-append
                                  (this-package-native-input "clang-toolchain")
                                  "/include")
                               (cond
                                ((getenv "CPATH")
                                 => search-path-as-string->list)
                                (else
                                 '())))
                         ":")))))))
      (home-page "https://github.com/tpoechtrager/cctools-port")
      (synopsis "Darwin's @code{cctools} and @code{ld64}")
      (description "This package provides a port of Darwin's
@code{cctools} (e.g. @code{install_name_tool}) and @code{ld64}.")
      (license license:apsl2))))
