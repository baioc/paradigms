#! /usr/bin/scheme --script

(let ([args (cdr (command-line))]) ; (cdr (command-line))
  (write args)
  (newline))
