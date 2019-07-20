;; blurring the line between data and procedure
(define (cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))

(define (car x) (x 1))

(define (cdr x) (x 2))

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
