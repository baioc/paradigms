;; primitives
42
0.03
12e-4
1e-90
9/15
3+4i
#\c
"this is a string"
'a
''a
'(a b c)
#t
true
#f
false

;; variables
(set! k -1)
k
(define k 99)
k
(set! k -88)
k
eq?
(eq? false 'false)
(eq? 'lambda 'λ)
(+ 1 2 3 4)

;; lambda & define && if
(define (test b) (if b (display "#TRUE\n") (display "#FALSE\n")))
(test '#f)
(test false)
; (define l0 (λ b b)) (l0 0)
(define l1 (lambda (b) b))
(define (l2 b) b)
(define l3 (lambda (b) (b)))
(define (l4)
  (l3 (l2 newline))
  (l3 (l1 newline))
  (square 4))

;; begin
(begin
  'hey
  (l4))
; (begin)
(begin "ho")

;; @TEST: order of evaluation
(define (x=1) (set! x 1) 1)
(define (x=2) (set! x 2) 2)
(define x 0)
x
(+ (x=1) (x=2))
x
(set! x 0)
x
(+ (x=2) (x=1))
x

;; let
(let ((x 7) (y 8)) (+ x y))
(let ((x 8) (y 7)) y)
(let ((x 7) (y 8)) (+ x y) y)

;; let*
(define (call1* f g)
  (let* ((b (g))
         (a (f)))
    a))
(call1* x=1 x=2)
(define (call2* f g)
  (let* ((b (g))
         (a (f)))
    (+ a b)))
(set! x 0)
x
(call2* x=1 x=2)
x
(set! x 0)
x
(call2* x=2 x=1)
x

;; cond
(cond ((assoc 'b '((a 1) (b 2))) => cadr))
(define (test flag)
  (cond (else 'other-option)
        (flag 'should-be-this)))
(cond )
(cond (else 1))

;; @TODO: internal definitions

;; @TODO: and

;; @TODO: or
