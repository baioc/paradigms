(define (square x) (* x x))

(define (print m) (display m) (newline))

(define (halve x) (ash x -1))
(define (double x) (ash x 1))

(define (maybe-car lst alt)
  (if (null? lst) alt
      (car lst)))

(define (average a b) (/ (+ a b) 2))

(define tolerance 1e-9)
