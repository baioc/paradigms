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

;; "dumb" fibonacci
(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 1))
         (fib (- n 2)))))

;; linear iteration / tail-end recursive fibonacci
(define (fibonacci n)
  (define (iter a b count)
    (if (= count 0) a
        (iter b (+ a b) (- count 1))))
  (iter 0 1 n))

;; takes a list as argument
(define (list-sum num-list)
  (if (null? num-list) 0
      (+ (car num-list) (list-sum (cdr num-list)))))

;; takes two or more "atom" numbers
(define (arg-sum x y . args)
  (+ x y (apply + args)))

;; if, else if example, could use cond instead
(define (expt base exponent)
  (if (> exponent 0)
        (* base (expt base (- exponent 1)))
      (if (= exponent 0) 1
          (/ 1 (expt base (* exponent -1))))))

;; uses or & and to be able to return early from the recursion
;; (cadr x) := (car (cdr x))
(define (sorted? num-list)
  (or (<= (length num-list) 1)
      (and (<= (car num-list) (cadr num-list))
           (sorted? (cdr num-list)))))

(define test '("naught" 5 1 2 3 4))
