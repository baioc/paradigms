;; equivalent to the sigma notation \sum_{i=a}^{b} term_i
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))


(define (average a b)
  (/ (+ a b) 2))

;; Heron's method for finding square roots
(define (sqrt x)
  (define tolerance 1e-5)

  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))

  (define (try guess)
    (if (good-enough? guess) guess
        (try (improve guess))))

  (try 1.0))

;; fixed-point of f is x such that f(x) = x
(define (fixed-point f start)
  (define tolerance 1e-5)

  (define (close-enough? u v)
    (< (abs (- u v)) tolerance))

  (define (iter old new)
    (if (close-enough? old new) new
        (iter new (f new))))

  (iter start (f start)))

;; takes a function as parameter and returns its dampened version
(define (average-damp f)
  (lambda (x) (average (f x) x)))

;; the same Heron's method, with more abstraction underneath
(define (sqrt x)
  (fixed-point
    (average-damp (lambda (y) (/ x y))) 1.0))

;; numerical derivative of a function
(define (deriv f)
  (define dx 1e-6)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

;; Newton's method
(define (Newton f guess)
  (fixed-point (lambda (x) (- x (/ (f x) ((deriv f) x))))
               guess))

;; square root yet again, but with Newton's method
(define (sqrt x)
  (Newton (lambda (y) (- x (square y))) 1.0))


;; iterative version O(n) of exponentiation
(define (expt b n)
  (define (iter b counter product)
    (if (= counter 0) product
        (iter b (- counter 1) (* b product))))
  (iter b n 1))

;; recursive O(lg(n)) version, using b^n = (b^(n/2))^2 (successive squaring)
(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))

;; iterative O(lg(n)) version, using (b^(n/2))^2 = (b^2)^(n/2)
(define (expt b n)
  (define (iter b n aux)
    (cond ((= n 0) aux)
          ((odd? n) (iter b (- n 1) (* b aux)))
          (else (iter (square b) (/ n 2) aux))))
  (if (< n 0) (/ 1 (expt b (- n)))
      (iter b n 1)))
