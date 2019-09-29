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
