#lang racket/base
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(module+ main
  (command-line
   #:usage-help
   "Adjusts Mach-O install names in library <filename>"
   #:once-each
   [("--llvm-objdump") path "use <path> for llvm-objdump"
                       (objdump path)]
   [("--install_name_tool") path "use <path> for install_name_tool"
                            (install_name_tool path)]
   #:args ([filename #f])
   (if filename
       (patch-darwin-dylib filename)
       (eprintf "~a: no filename given: doing nothing\n" who))))

(require racket/match
         racket/port
         racket/pretty
         racket/system
         racket/cmdline)

(define objdump
  (make-parameter (find-command "llvm-objdump")))
(define install_name_tool
  (make-parameter (find-command "install_name_tool")))
(define who 'patch-darwin-dylib)


(define (patch-darwin-dylib filename)
  (check-filename filename)
  (patch-id filename)
  (patch-libiconv filename))

(define (patch-id filename)
  (invoke (install_name_tool) "-id" filename filename))

(define (patch-libiconv filename)
  (define dylibs-used
    (with-output-to-string
      (λ ()
        (invoke (objdump) "--macho" "--dylibs-used" filename))))
  (invoke (install_name_tool)
          "-change"
          (match dylibs-used
            [(pregexp #px"(?<=\\s)/nix/store/\\S+/libiconv[\\d\\.]*\\.dylib(?=\\s)"
                      (list nix-lib))
             nix-lib])
          "/usr/lib/libiconv.2.dylib"
          filename))

(define (check-filename filename)
  (unless (path-string? filename)
    (raise-argument-error who "path-string?" filename))
  (let-values ([{base name dir}
                (split-path filename)])
    (when (or (not (eq? 'relative base))
              dir)
      (raise-argument-error who
                            "a filename matching the desired Mach-O library name"
                            filename)))
  (unless (file-exists? filename)
    (raise-arguments-error who "given file does not exist in the current directory"
                           "file" filename
                           "directory" (current-directory))))

(define (invoke program . args)
  (when (procedure? program)
    (program))
  (eprintf ";; invoking ...\n")
  (pretty-print (cons program args) (current-error-port))
  (match (parameterize ([current-input-port (open-input-bytes #"")])
           (apply system*/exit-code program args))
    [0
     (void)]
    [code
     (error 'invoke "command ~e failed (code ~e)" program code)]))

(define-syntax-rule (find-command cmd)
  (or (find-executable-path cmd)
      (λ () (error who "~a: command not found" cmd))))
