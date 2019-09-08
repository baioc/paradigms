;; equivalent to the sigma notation \sum_{i=a}^{b} term_i
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))


(define (average a b)
  (/ (+ a b) 2))

(define (square x)
  (* x x))

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

;; recursive O(n) multiplication
(define (* a n)
  (if (= n 0) 0
      (+ a (* a (- n 1)))))

;; iterative O(n) multiplication
(define (* a n)
  (define (iter a n prod)
    ; (display a) (display n) (display prod) (newline)
    (if (= n 0) prod
        (iter a (- n 1) (+ prod a))))
  (if (< n 0) (iter (- a) (- n) 0)
      (iter a n 0)))

;; Russian peasant multiplication, O(lg(n))
(define (* a n)
  (define (double b) (+ b b))
  (define (halve b) (quotient b 2))
  (define (iter a n prod)
    ; (display a) (display n) (display prod) (newline)
    (cond ((= n 0) prod)
          ((even? n) (iter (double a) (halve n) prod))
          (else (iter a (- n 1) (+ prod a)))))
  (if (< n 0) (iter (- a) (- n) 0)
      (iter a n 0)))

;; iterative O(lg(n)) fibonacci
(define (fib n)
  (define (iter a b p q n)
    (cond ((= n 0) b)
          ((even? n) (iter a
                           b
                           (+ (square q) (square p)) ; p'
                           (+ (square q) (* 2 (* p q))) ; q'
                           (/ n 2)))
          (else (iter (+ (* b q) (* a (+ q p)))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- n 1)))))
  (iter 1 0 0 1 n))


(define (deriv exp var)
  (define (variable? x)
    (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (make-sum augend addend)
    (cond ((and (number? addend) (number? augend)) (+ augend addend))
          ((and (number? augend) (= augend 0)) addend)
          ((and (number? addend) (= addend 0)) augend)
          (else (list '+ augend addend))))
  (define (sum? x)
    (and (list? x) (eq? (car x) '+)))
  (define (augend s)
    (cadr s))
  (define (addend s)
    (caddr s))
  (define (make-product multiplier multiplicand)
    (define (=number? exp num)
      (and (number? exp) (= exp num)))
    (cond ((or (=number? multiplier 0)
               (=number? multiplicand 0))
            0)
          ((and (number? multiplier)
                (number? multiplicand))
            (* multiplier multiplicand))
          ((=number? multiplier 1) multiplicand)
          ((=number? multiplicand 1) multiplier)
          (else (list '* multiplier multiplicand))))
  (define (product? x)
    (and (list? x) (eq? (car x) '*)))
  (define (multiplier p)
    (cadr p))
  (define (multiplicand p)
    (caddr p))
  ; todo: other rules
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (augend exp) var)
                   (deriv (addend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define foo
  '(+ (+ (* a (* x x))
         (* b x))
      c))


(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((divides? test-divisor n) test-divisor)
        ((> (square test-divisor) n) n)
        (else (find-divisor n (+ test-divisor 1)))))
