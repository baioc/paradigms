(load "common.scm")


;; Heron's method for finding square roots
(define (sqrt x)
  (define (try guess)
    (if (good-enough? guess) guess
        (try (improve guess))))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))

  (try 1.0))


;; fixed-point of f is x such that f(x) = x
(define fixpoint
  (case-lambda
    ((f x) (fixpoint f x 1e-9))
    ((f x tolerance)
       (letrec ((approx?
                  (lambda (a b)
                    (< (abs (- a b)) tolerance)))
                (try
                  (lambda (old new)
                    (if (approx? old new) new
                        (try new (f new))))))
         (try x (f x))))))

(define (phi-rat tol)
  (fixpoint
    (lambda (rat)
      (let ((fcurr (numerator rat))
            (fprev (denominator rat)))
        (/ (+ fcurr fprev) fcurr)))
    1/1
    tol))

(define phi (exact->inexact (phi-rat 1e-15))) ;; => 102334155/63245986 = fib(40)/fib(39)

;; takes a function as parameter and returns its dampened version
(define (average-damp f)
  (lambda (x) (average (f x) x)))

;; the same Heron's method, with more abstraction underneath
(define (sqrt x)
  (fixpoint (average-damp (lambda (y) (/ x y))) 1.0))


;; numerical derivative of a function
(define (deriv f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

;; Newton's method
(define root
  (case-lambda
    ((f x) (root f x 1e-8))
    ((f x dx)
     (let ((df (deriv f dx)))
       (fixpoint (lambda (x) (- x (/ (f x) (df x)))) x dx)))))


;; square root yet again, but with Newton's method
(define (sqrt x)
  (root (lambda (y) (- x (square y))) 1.0))

(define pi (root sin 3))

(define phi (root (lambda (x) (+ (square x) (- x) -1)) 1.0 1e-11))
