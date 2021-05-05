(import (scheme base))
(import (srfi 95))
(import (scheme write))

;; Returns GCD(a,b) and Bezout coefficients s,t such that a*s + b*t = gcd(a, b)
(define (egcd a b)
  (let loop ((r0 a) (r1 b)
             (s0 1) (s1 0)
             (t0 0) (t1 1))
    (write `((r ,r0) (s ,s0) (t ,t0))) (newline)
    (if (zero? r1)
        (values r0 s0 t0)
        (let ((q (quotient r0 r1)))
          (loop r1 (- r0 (* q r1))
                s1 (- s0 (* q s1))
                t1 (- t0 (* q t1)))))))

;; Computes the modular inverse of n, mod m.
(define (invmod n m)
  (let-values (((gcd s t) (egcd n m)))
    (and (= gcd 1)
         (modulo s m))))

;; Gives a solution to equations in the form x = a (mod p), where ps are all
;; mutually coprimes. Expects a list of as and a list of ps of equal length.
(define (crt as ps)
  ;; solves two-moduli system through sieving search
  (define (crt2 a1 p1 a2 p2)
    (write `((,a1 . ,p1) (,a2 . ,p2))) (newline)
    (let loop ((k 0))
      (let ((x (+ a1 (* k p1))))       ; <- x = a1 (mod p1)
        (display "  ") (write `((x ,x) (r ,(modulo x p2)))) (newline)
        (cond ((= (modulo x p2) a2) x) ; -> x = a2 (mod p2)
              ((< x (* p1 p2)) (loop (+ k 1)))
              (else (error "invalid system" `((,a1 . ,p1) (,a2 . ,p2))))))))
  ;; each equation a cons cell (a . p), 0 <= a < p
  (define (*crt first rest)
    (if (null? rest)
        (car first)
        (let ((a1 (car first)) (p1 (cdr first))
              (a2 (caar rest)) (p2 (cdar rest)))
          (let ((x (crt2 a1 p1 a2 p2)) (n (* p1 p2))) ; solution = x (mod n)
              (*crt (cons x n) (cdr rest))))))
  ;; make sure these are actually remainders and sort for better performance
  (let* ((as (map modulo as ps))
         (eqs (sort! (map cons as ps) > cdr)))
    (*crt (car eqs) (cdr eqs))))

;; Empowers a monoid for fast exponentiation through successive squaring.
(define (empower operation neutral)
  (lambda (basis power)
    (let ((square (lambda (x) (operation x x))))
      (let loop ((b basis) (n power) (acc neutral))
        (write `((b ,b) (n ,n) (acc ,acc))) (newline)
        (cond ((zero? n) acc)
              ((even? n) (loop (square b) (/ n 2) acc))
              (else (loop b (- n 1) (operation b acc))))))))

;; Modular exponentiation: b^n (mod m)
(define (modpow b n m)
  ((empower (lambda (x y) (modulo (* x y) m)) 1) b n))

;; Computes the numbers in (1 ... n) which are n-prime.
(define (totient n)
  (if (= n 1)
      '(1)
      (let loop ((k (- n 1)) (coprimes '()))
        (cond ((<= k 0) coprimes)
              ((= 1 (gcd k n)) (loop (- k 1) (cons k coprimes)))
              (else (loop (- k 1) coprimes))))))

;; Test the primality of a number.
(define (prime? n)
  (cond ((< n 2) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else (let loop ((a 2))
          (cond ((>= a (- n 1)) #t)
                ((= 1 (modpow a (- n 1) n)) (loop (+ a 1)))
                (else #f))))))

;; Evaluates a polynomial (a0 + ... + an*x^n) at a given point.
(define (horner x pn)
  (if (null? (cdr pn))
      (car pn)
      (+ (car pn) (* x (horner x (cdr pn))))))

;; Builds a lambda that evaluates the polynomial defined by given coefficients.
(define (make-poly a0 . ans)
  (let ((pn (cons a0 ans)))
    (lambda (x) (horner x pn))))

;; Fast fibonacci through successive squaring of polynomials mod (X^2 - X - 1).
(define (fib n)
  (let loop ((p 0) (q 1) (n n) (a 1) (b 0))
    (write `((n ,n) (a ,a) (b ,b) (p ,p) (q ,q))) (newline)
    (cond ((<= n 0) b)
          ((even? n)
           (let ((q2 (* q q)))
             (loop (+ q2 (* p p))
                   (+ q2 (* 2 p q))
                   (/ n 2) a b)))
          (else
           (loop p q (- n 1)
                 (+ (* a (+ q p)) (* b q))
                 (+ (* a q) (* b p)))))))
