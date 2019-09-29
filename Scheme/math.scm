;; equivalent to the sigma notation \sum_{i=a}^{b} term_i
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))


(define tolerance 1e-15)
(define (average a b) (/ (+ a b) 2))
(define (square x) (* x x))
(define (halve x) (ash x -1))
(define (double x) (ash x 1))
(define (maybe-car lst alt)
  (if (null? lst) alt
      (car lst)))


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
(define (fixpoint f x . opts-tol)
  (let* ((tolerance (maybe-car opts-tol 1e-9))
         (approx? (lambda (a b) (< (abs (- a b)) tolerance))))
    (let try ((old x) (new (f x)))
      (if (approx? old new) new
          (try new (f new))))))

;; takes a function as parameter and returns its dampened version
(define (average-damp f)
  (lambda (x) (average (f x) x)))

;; the same Heron's method, with more abstraction underneath
(define (sqrt x)
  (fixpoint (average-damp (lambda (y) (/ x y))) 1.0))

;; Newton's method
(define (root f x . opts-tol) ;; metodo de Newton
  (let* ((dx (maybe-car opts-tol 1e-8))
         (df (deriv f dx)))
    (fixpoint (lambda (x) (- x (/ (f x) (df x)))) x dx)))

;; numerical derivative of a function
(define (deriv f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

;; square root yet again, but with Newton's method
(define (sqrt x)
  (root (lambda (y) (- x (square y))) 1.0))


;; slow exponentiation O(2^(n-1)) for base two
;; 2^n = 2 * 2^(n-1) = 2^(n-1) + 2^(n-1)
(define (2^ n)
  (cond ((= n 0) 1)
        ((= n 1) 2)
        (else (+ (2^ (- n 1))
                 (2^ (- n 1))))))

;; recursive O(lg(n)) version, via successive squaring
;; b^n = (b^(n/2))^2
(define (^ b n)
  (cond ((= n 0) 1)
        ((even? n) (square (^ b (halve n))))
        (else (* b (^ b (- n 1))))))

;; iterative successive squaring
;; b^n = (b^2)^(n/2)
(define (^ b n)
  (define (iter b n prod)
    (cond ((= n 0) prod)
          ((even? n) (iter (square b) (halve n) prod))
          (else (iter b (- n 1) (* b prod)))))
  (if (< n 0)
      (iter (/ 1 b) (- n) 1)
      (iter b n 1)))

;; recursive O(n) multiplication
(define (times a n)
  (if (= n 0) 0
      (+ a (times a (- n 1)))))

;; iterative O(n) multiplication
(define (times a n)
  (define (iter a n prod)
    ; (display a) (display n) (display prod) (newline)
    (if (= n 0) prod
        (iter a (- n 1) (+ prod a))))
  (if (< n 0) (iter (- a) (- n) 0)
      (iter a n 0)))

;; Russian peasant multiplication, O(lg(n))
(define (times a n)
  (define (iter a n prod)
    ; (display a) (display n) (display prod) (newline)
    (cond ((= n 0) prod)
          ((even? n) (iter (double a) (halve n) prod))
          (else (iter a (- n 1) (+ prod a)))))
  (if (< n 0)
      (iter (- a) (- n) 0)
      (iter a n 0)))


(define (pow power operation basis neutral succession)
  (let iter ((b basis) (n power) (acc neutral))
    (cond ((= n 0) acc)
          ((even? n) (iter (succession b) (halve n) acc))
          (else (iter b (- n 1) (operation b acc))))))

(define (empower operation neutral . opts-succ)
  (let ((succession (maybe-car opts-succ (lambda (x) (operation x x)))))
    (lambda (basis power)
      (pow power operation basis neutral succession))))

(define times (empower + 0))

(define (mul b n)
  (if (< n 0)
      (times (- b) (- n))
      (times b n)))

(define (^ b n)
  (let ((raise (empower * 1)))
    (if (< n 0)
        (raise (/ 1 b) (- n))
        (raise b n))))

(define (fibonacci n)
  (let ((fn (cadr (pow ;; nth power
                       (abs n)
                       ;; of the binary operation / transformation
                       (lambda (coefs fibs)
                         (let ((p (car coefs)) (q (cadr coefs))
                               (a (car fibs)) (b (cadr fibs)))
                           (list (+ (* b q) (* a (+ q p)))
                                 (+ (* b p) (* a q)))))
                       ;; starting from basis
                       '(0 1)
                       ;; accumulated over
                       '(1 0)
                       ;; where the successive squaring that empowers
                       ;; the operation is given by
                       (lambda (coefs)
                         (let ((p (car coefs)) (q (cadr coefs)))
                           (list (+ (square q) (square p))
                                 (+ (square q) (* 2 (* p q))))))))))
    ;; negafibonacci
    (if (and (< n 0)
             (even? n))
        (- fn)
        fn)))

;; matrix fibonacci
(define (fibona n) ;; n > 0
  (define (mul-matrix-2x2 A B)
    (let ((a11 (caar A)) (a12 (cadar A))
          (a21 (caadr A)) (a22 (cadadr A))
          (b11 (caar B)) (b12 (cadar B))
          (b21 (caadr B)) (b22 (cadadr B)))
      (list (list (+ (* a11 b11) (* a12 b21))
                  (+ (* a11 b12) (* a12 b22)))
            (list (+ (* a21 b11) (* a22 b21))
                  (+ (* a21 b12) (* a22 b22))))))
  (let ((nth-transform (empower mul-matrix-2x2
                                '((1 0)
                                  (0 1)))))
    (caar
      (nth-transform '((1 1)
                       (1 0))
                     (- n 1)))))

;; closed form through eigen-stuff
(define sqrt5 (sqrt 5))
(define phi (/ (+ 1 sqrt5) 2)) ;; golden ration
(define fi (- 1 phi)) ;; golden ratio complement
(define (fibonac n)
  (round (/ (- (^ phi n) (^ fi n))
            (sqrt 5))))


(define (diff exp var)
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
         (make-sum (diff (augend exp) var)
                   (diff (addend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (diff (multiplicand exp) var))
           (make-product (diff (multiplier exp) var)
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
