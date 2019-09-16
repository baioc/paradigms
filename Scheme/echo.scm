#! /usr/bin/guile -s
; !#

(let ([args (cdr (command-line))])
  (for-each (lambda (arg)
              (display arg)
              (display " "))
            args)
  (newline))
