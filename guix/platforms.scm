(define-module (platforms)
  #:export (non-apple-platforms))

(define non-apple-platforms
  ;;     (listof (list/c string? string?))
  ;; where the first is the Racket name and the second is a name
  ;; suitable for (%current-system) or (%current-target-system)
  `(("x86_64-linux" "x86_64-linux")
    ("x86_64-win32" "x86_64-w64-mingw32")
    ("i386-win32"   "i686-w64-mingw32")))
