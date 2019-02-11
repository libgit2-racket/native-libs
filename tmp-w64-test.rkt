#lang at-exp racket/base

(require ffi/unsafe)

(ffi-lib @string->path{C:\projects\native-libgit2-pkgs\build\win32\x86_64\Debug\git2.dll}
         '(#f))
