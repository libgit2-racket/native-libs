(define-module (platforms)
  #:use-module (guix gexp)
  #:export (non-apple-platforms
            make-cfg-flags-mingw))

(define non-apple-platforms
  ;;     (listof (list/c string? string?))
  ;; where the first is the Racket name and the second is a name
  ;; suitable for (%current-system) or (%current-target-system)
  `(("x86_64-linux" "x86_64-linux")
    ("x86_64-win32" "x86_64-w64-mingw32")
    ("i386-win32"   "i686-w64-mingw32")))

(define-public (make-cfg-flags-mingw sys)
  #~`("-DCMAKE_C_FLAGS=-static-libgcc"
      "-DCMAKE_CXX_FLAGS=-static-libgcc"
      "-DCMAKE_EXE_LINKER_FLAGS=-static-libgcc"
      "-DCMAKE_MODULE_LINKER_FLAGS=-static-libgcc"
      #$(string-append "-DCMAKE_RC_COMPILER=" sys "-windres")
      #$(string-append "-DDLLTOOL=" sys "-dlltool")))
