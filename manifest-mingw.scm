(use-modules (guix)
             (guix packages)
             (guix profiles)
             (ice-9 match)
             (srfi srfi-1)
             (gnu packages version-control))

(define target
  (or (getenv "RKT_LIBGIT2_TARGET")
      "i686-w64-mingw32"))

(packages->manifest
 (filter-map
  (match-lambda
    ((_ (? package? p))
     p)
    ((_ (? package? p) out)
     (list p out))
    (_
     #f))
  (bag-transitive-inputs
   (package->bag
    (package
      (inherit libgit2)
      (inputs `())
      (propagated-inputs `()))
    (%current-system)
    target))))
