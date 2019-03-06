;; run with: kawa -f script.scm -s
;; '-s' flag makes it stay in REPL mode after evaluating the definitions
;; call (exit 0) to leave

(define (fact n)
	(if (< n 1) 1
		(* n (fact (- n 1)))))

(define (fib n)
	(if (<= n 1) 1
		(+ (fib (- n 1)) (fib (- n 2)))))

;; takes a list
(define (sum num-list)
	(if (null? num-list) 0
		(+ (car num-list) (sum (cdr num-list)))))

;; takes two or more numbers
(define (addup x y . args)
	(+ x y (apply + args)))

;; if, else if example, could use cond instead
(define (** base exponent)
	(if (> exponent 0)
		(*	base (** base (- exponent 1)))
		(if (= exponent 0) 1
			(/ 1 (** base (* exponent -1))))))

;; uses or & and to be able to return early from the recursion
;; (cadr x) <=> (car (cdr x))
(define (sorted? num-list)
	(or (<= (length num-list) 1)
		(and (<= (car num-list) (cadr num-list))
			(sorted? (cdr num-list)))))

(define test '("naught" 5 1 2 3 4))
