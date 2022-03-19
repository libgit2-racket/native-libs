(define-module (platforms)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (non-apple-platforms
            cfg-flags-windows-jsexpr
            make-cfg-flags-mingw))

(define non-apple-platforms
  ;;     (listof (list/c string? string?))
  ;; where the first is the Racket name and the second is a name
  ;; suitable for (%current-system) or (%current-target-system)
  `(("x86_64-linux" "x86_64-linux")
    ("x86_64-win32" "x86_64-w64-mingw32")
    ("i386-win32"   "i686-w64-mingw32")))

(define cfg-flags-windows-spec
  `(("-DCMAKE_RC_COMPILER=" <> "-windres")
    ("-DDLLTOOL=" <> "-dlltool")
    "-DCMAKE_C_FLAGS=-static-libgcc"
    "-DCMAKE_CXX_FLAGS=-static-libgcc"
    "-DCMAKE_EXE_LINKER_FLAGS=-static-libgcc"
    "-DCMAKE_MODULE_LINKER_FLAGS=-static-libgcc"))

(define (interp-flags-spec sys)
  (map (match-lambda
         ((? string? flag)
          flag)
         ((pre '<> post)
          (string-append pre sys post)))
       cfg-flags-windows-spec))

(define (make-cfg-flags-mingw sys)
  #~`(#$@(interp-flags-spec sys)))

(define cfg-flags-windows-jsexpr
  (interp-flags-spec "${arch}-w64-mingw32"))
