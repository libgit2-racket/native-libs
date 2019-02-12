#lang at-exp racket/base

(require ffi/unsafe)

(ffi-lib @string->path{C:\projects\native-libgit2-pkgs\libgit2-win32-x86_64\git2.dll}
         '(#f))
