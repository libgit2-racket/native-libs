#lang _-exp racket

(module+ main
  (require yaml
           racket/runtime-path)
  (define-runtime-path .appveyor.yml
    ".appveyor.yml")
  (call-with-output-file* .appveyor.yml
    #:exists 'truncate/replace
    (λ (out)
      (write-yaml #:scalar-style #\| ;; to avoid a bug
                  document
                  out))))

(define s
  (case-lambda
    [(single) single]
    [args
     (string-normalize-spaces
      (string-append* args))]))

(define linux-install-racket
  (list ƒs{git clone --branch i386
 https://github.com/liberalartist/travis-racket.git
 ~/travis-racket}
        ;; pipe to bash not sh!
        ƒs{cat ~/travis-racket/install-racket.sh | bash}
        ;; install-racket.sh can't set for us
        ƒs{export PATH="${RACKET_DIR}/bin:${PATH}"}))

(define windows-install-racket
  (list "git clone https://github.com/liberalartist/appveyor-racket.git C:\a-r"
        ƒs{bash C:\a-r\download-racket.sh C:\install-racket.exe}
        ƒs{C:\install-racket.exe /S /D=C:\racket}
        ƒs{set PATH=%PATH%;C:\racket\}))

(define variants
  (for*/list ([windows? (in-list '(#t #f))]
              [64bit? (in-list '(#t #f))])
    (define image
      (if windows? "Visual Studio 2015" "Ubuntu"))
    (define platform
      (if 64bit? "x64" "x86"))
    (define pkg-dir
      (let ([arch (if 64bit? "x86_64" "i386")])
        (string-append "libgit2-"
                       (if windows? "win32" arch)
                       "-"
                       (if windows? arch "linux"))))
    `#hash(["matrix" . #hash(["only" . (#hash(["image" . ,image]
                                              ["platform" . ,platform]))])]
           ["environment" . #hash(["PKG_DIR" . ,pkg-dir])]
           ;; get the src submodule
           ["install" . ("git submodule init"
                         "git submodule update")]
           ;; install Racket
           ["before_build" . ,(if windows?
                                  windows-install-racket
                                  linux-install-racket)]
           ;; build libgit2
           ["build_script" . ("racket make-libgit2.rkt")]
           ;; install the built package
           ["before_test" . (,(string-append "raco pkg install -i --link "
                                             (if windows?
                                                 "%PKG_DIR%"
                                                 ƒs{"$PKG_DIR"})))]
           ;; test that the package works
           ["test_script" . ("test-script.rkt")])))

(define document
  `#hash(["image" . ("Visual Studio 2017")] ;; !!!
         ["platform" . ("x64" "x86")]
         ["environment"
          . #hash(["RACKET_VERSION" . "7.2"]
                  ["CYGPATH" . "cygpath -u"] ;; used by download-racket.sh on Windows
                  ["RACKET_DIR" . "~/racket"])] ;; used by install-racket.sh on Ubuntu
         ["for" . ,variants]
         ["artifacts" . (#hash(["path" . "$(PKG_DIR)"]))]))
       



