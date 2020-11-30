(define-library (debug)
  (export
    ;; If debugging is currently enabled for the given symbol, this prints, to
    ;; the (current-error-port) and as if using display, the input object.
    debug-display

    ;; Adds levels of indentation to the log, useful for debugging recursion.
    debug-indent

    ;; Equivalent to (debug-display symbol "\n")
    debug-newline

    ;; Enables debug-print for given symbols.
    start-debugging!

    ;; Disables debug-print for the given symbols, or for everithing when given
    ;; no arguments.
    stop-debugging!)

  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (srfi 113) (srfi 128)) ; sets and bags + comparators

  (begin
    (define *debug-symbols* (set (make-eq-comparator)))

    (define (debugging? symbol)
      (set-member *debug-symbols* symbol #f))

    (define (debug-display symbol object)
      (when (debugging? symbol)
        (display object (current-error-port))))

    (define (debug-indent symbol n)
      (do ((i 0 (+ i 1)))
          ((>= i n))
        (debug-display symbol "    ")))

    (define (debug-newline symbol)
      (debug-display symbol "\n"))

    (define start-debugging!
      (lambda symbols
        (set! *debug-symbols* (apply set-adjoin! *debug-symbols* symbols))))

    (define stop-debugging!
      (case-lambda
        (() (set! *debug-symbols* (set (make-eq-comparator))))
        (symbols (set! *debug-symbols* (apply set-delete! *debug-symbols* symbols)))))))
