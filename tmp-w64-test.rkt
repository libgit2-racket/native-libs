#lang at-exp racket/base

(require ffi/unsafe)

(define-syntax-rule (s str) str)

(ffi-lib @s{C:\projects\native-libgit2-pkgs\build\win32\x86_64\Debug\git2.dll}
         '(#f))
