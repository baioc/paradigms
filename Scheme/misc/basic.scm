;; linear recursion factorial
(define (fact n)
  (if (< n 1) 1
      (* n (fact (- n 1)))))

;; linear iteration factorial
(define (factorial n)
  (define (iter product counter)
    (if (> counter n) product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; "dumb" binary-recursive fibonacci
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; linear iteration / tail-recursive fibonacci
(define (fibo n)
  (let iter ((n n) (prev 1) (curr 0))
    (if (= n 0) curr
        (iter (- n 1) curr (+ prev curr)))))

;; the Euclidean algorithm
(define (gcd a b)
  (if (= b 0) a
      (gcd b (modulo a b))))


;; takes a list as argument
(define (list-sum num-list)
  (if (null? num-list) 0
      (+ (car num-list) (list-sum (cdr num-list)))))

;; takes two or more "atom" numbers
(define (arg-sum x y . args)
  (+ x y (list-sum args)))

;; if, else if example, could use cond instead
(define (expt base exponent)
  (if (> exponent 0)
      (* base (expt base (- exponent 1)))
      (if (= exponent 0)
          1
          (/ 1 (expt base (* exponent -1))))))

;; uses or & and to be able to return early from the recursion
;; (cadr x) := (car (cdr x))
(define (sorted? sequence)
  (or (<= (length sequence) 1)
      (and (<= (car sequence) (cadr sequence))
           (sorted? (cdr sequence)))))


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


; (insert 4 123335667 >) ; => 1233345667
(define (insert x seq less?)
  (define (null? x) (zero? x))
  (define (list x) x)
  (define (car x) (remainder x 10))
  (define (cdr x) (quotient x 10))
  (define (cons d x) (+ (* x 10) d))
  (cond ((null? seq) (list x))
        ((not (less? (car seq) x)) (cons x seq))
        (else (cons (car seq) (insert x (cdr seq) less?)))))
