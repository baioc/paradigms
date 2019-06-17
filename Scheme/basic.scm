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
		(+ (fib (- n 1)) (fib (- n 2)))))

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

;; equivalent to the sigma notation \sum_{i=a}^{b} term_i
(define (sum term a next b)
	(if (> a b) 0
		(+ (term a)
		   (sum term (next a) next b))))

;; if, else if example, could use cond instead
(define (** base exponent)
	(if (> exponent 0)
		(* base (** base (- exponent 1)))
		(if (= exponent 0) 1
			(/ 1 (** base (* exponent -1))))))

;; uses or & and to be able to return early from the recursion
;; (cadr x) <=> (car (cdr x))
(define (sorted? num-list)
	(or (<= (length num-list) 1)
		(and (<= (car num-list) (cadr num-list))
			(sorted? (cdr num-list)))))

(define (average a b)
	(/ (+ a b) 2))

;; Heron's method for finding square roots
(define (sqrt x . opt)
	(define (improve guess)
		(average guess (/ x guess)))

	(define (good-enough? guess)
		(< (abs (- (* guess guess) x))
			(if (null? opt) 1e-10 (car opt))))

	(define (try guess)
		(if (good-enough? guess) guess
			(try (improve guess))))

	(try 1.0))

;; fixed-point of f is x such that f(x) = x
(define (fixed-point f start)
	(define tolerance 0.00001)
	(define (close-enough? u v)
		(< (abs (- u v)) tolerance))
	(define (iter old new)
		(if (close-enough? old new) new
			(iter new (f new))))
	(iter start (f start)))

;; average-damp define notation
(define (average-damp f)
	(define (foo x)
		(average (f x) x))
	foo)

;; average-damp with lambda notation
(define average-damp
	(lambda (f)
		(lambda (x) (average (f x) x))))

;; the same method. with more abstraction underneath
(define (sqrt x)
	(fixed-point
		(average-damp (lambda (y) (/ x y))) 1.0))

(define test '("naught" 5 1 2 3 4))
