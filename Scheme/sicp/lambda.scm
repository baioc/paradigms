;; blurring the line between data and procedure
; (define (cons a b)
;   (lambda (pick)
;     (cond ((= pick 1) a)
;           ((= pick 2) b))))

; (define (car x) (x 1))

; (define (cdr x) (x 2))


;; "Alonzo Church's hack", with mutable data
(define (cons x y)
  (lambda (m)
    (m x
       y
       (lambda (n) (set! x n))
       (lambda (n) (set! y n)))))

(define (car x)
  (x (lambda (a d set-a! set-d!) a)))

(define (cdr x)
  (x (lambda (a d set-a! set-d!) d)))

(define (set-car! p v)
  (p (lambda (a d set-a! set-d!) (set-a! v))))

(define (set-cdr! p v)
  (p (lambda (a d set-a! set-d!) (set-d! v))))


(define (fac n)
  (define f fac)
  (if (= n 0) 1 (* n (f (- n 1)))))

(define (facto n)
  (define f facto)
  (define (aux n)
    (if (= n 0) 1 (* n (f (- n 1)))))
  (aux n))

(define (factor n)
  (define (aux f n)
    (if (= n 0) 1 (* n (f (- n 1)))))
  (aux factor n))

(define (factori n)
  (define (aux f)
    (lambda (n)
      (if (= n 0) 1 (* n (f (- n 1))))))
  ((aux factori) n))

(define (factoria n)
  (define (aux f)
    (lambda (n)
      (if (= n 0) 1 (* n (f (- n 1))))))
  (define (rec f)
    (lambda (n)
      ((aux (f f)) n)))
  (let ((fact (rec rec)))
    (fact n)))


(define (fix f)
  (define (g x)
    (lambda (arg)
      ((f (x x)) arg)))
  (g g))


(define fact
  (fix (lambda (f)
         (lambda (n)
           (if (= n 0) 1 (* n (f (- n 1))))))))
