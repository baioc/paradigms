;; primitives
42 ; 42
0.03 ; 0.03
12e-4 ; 0.0012
1e-90 ; 1(.0)e-90
#\c ; #\c
"this is a string" ; "this is a string"
'a ; a
''a ; (quote a)
'(a b c) ; (a b c)
#(1 2 3) ; #(1 2 3)
#t ; Unbound variable #t
true ; true
#f ; Unbound variable #f
false ; false

;; variables
(set! k -1) ; Unbound variable k
k ; Unbound variable k
(define k 99) ; *void*
k ; 99
(set! k -88) ; *void*
k ; -88
eq? ; #<procedure eq?>
(eq? false 'false) ; true
(eq? 'lambda 'λ) ; true
(+ 1 2 3 4) ; 10

;; lambda & define && if
(define (test b)
  (if b (display "#TRUE\n") (display "#FALSE\n"))) ; *void*
(test '#f) ; #TRUE\n#<void>
(test false) ; #FALSE\n#<void>
; (define l0 (λ b b)) (l0 0) ; Improper formal argument list b
(define l1 (lambda (b) b)) ; *void*
(define (l2 b) b) ; *void*
(define l3 (lambda (b) (b))) ; *void*
(define (l4)
  (l3 (l2 newline))
  (l3 (l1 newline))
  (square 4)) ; *void*

;; begin
(begin
  'hey
  (l4)) ; \n\n16
; (begin) ; Empty symbolic expression ()
(begin "hi") ; "hi"

;; order of evaluation
(define (x=1) (set! x 1) 1) ; *void*
(define (x=2) (set! x 2) 2) ; *void*
(define x 0) ; *void*
x ; 0
(+ (x=1) (x=2)) ; 3
x ; 2
(set! x 0)
x ; 0
(+ (x=2) (x=1)) ; 3
x ; 1

;; let
(let ((x 7) (y 8)) (+ x y)) ; 15
(let ((x 8) (y 7)) y) ; 7
(let ((x 7) (y 8)) (+ x y) y) ; 8

;; let*
(define (call1* f g)
  (let* ((b (g))
         (a (f)))
    x)) ; *void*
(call1* x=1 x=2) ; 1
(define (call2* f g)
  (let* ((b (g))
         (a (f)))
    (+ a b))) ; *void*
(set! x 0) ; *void*
x ; 0
(call2* x=1 x=2) ; 3
x ; 1
(set! x 0) ; *void*
x ; 0
(call2* x=2 x=1) ; 3
x ; 2

;; cond
(cond ((assoc 'b '((a 1) (b 2))) => cadr)) ; 2
(define (test flag)
  (cond (else 'other-option)
        (flag 'should-be-this))) ; ELSE clause isn't last
(test true) ; other-option
(cond ) ; false
(cond (else 1)) ; 1

;; and
(and) ; true
(and false) ; false
(and 'a true) ; true
(and true 'b) ; b

;; or
(or) ; false
(or false) ; false
(or false true) ; true
(or 'a false) ; a
(or false 'b) ; b

; internal definitions
(define (f x)
  (define (even? n)
    (if (= n 0) true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0) false
        (even? (- n 1))))
  (cond ((even? x) "EVEN")
        ((odd? x) "ODD")
        (else "WHAT"))) ; *void*
(f 0) ; "EVEN"
(f 1) ; "ODD"
(f 2) ; "EVEN"
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10)) ; 16
