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

;; if then else example, could use cond
(define (^ base exponent)
	(if (> exponent 0)
		(*	base (^ base (- exponent 1)))
		(if (= exponent 0) 1
			(/ 1 (^ base (* exponent -1))))))

;; cond works like a switch-case but with conditions & consequences
;; else works only with cond and always evaluates to true, like a default case
(define (flatten sequence)
	(cond ((null? sequence) '())
		((list? (car sequence))
			(append (flatten (car sequence)) (flatten (cdr sequence))))
		(else (cons (car sequence) (flatten (cdr sequence))))))

;; uses or & and to be able to return early from the recursion
;; (cadr x) <=> (car (cdr x))
(define (sorted? num-list)
	(or (<= (length num-list) 1)
		(and (<= (car num-list) (cadr num-list))
			(sorted? (cdr num-list)))))

;; function overwrite; the other 'sorted?' can't be called now
;; cmpfn is the function that checks order between two elements in the sequence
;; possible examples are '<' '<=' '>' '>=' ... 'string<?' and so on
(define (sorted? seq cmpfn)
	(or (<= (length seq) 1)
		(and (cmpfn (car seq) (cadr seq))
			(sorted? (cdr seq) cmpfn))))

;; use test as a list parameter to these functions
;; '!' is shorthand to 'define', as in declaring a variable
(! test '("naught" (5 ((6)) 7) 1 () 2 3 4))
