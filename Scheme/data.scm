;; blurring the line between data and procedure
(define (cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))

(define (car x) (x 1))

(define (cdr x) (x 2))

;; equivalent to python's range(n)
(define (iota n)
  (define (aux k)
    (if (< k n) (cons k (aux (+ k 1))) '()))
  (aux 0))

;; recursively map a procedure to a list's elements
(define (map proc list)
  (if (null? list) list
      (cons (proc (car list))
            (map proc (cdr list)))))

;; iterate on a list
(define (for-each proc list)
  (cond ((null? list) "")
        (else (proc (car list))
              (for-each proc (cdr list)))))

(define (filter predicate sequence)
  (cond ((null? sequence) sequence)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
