(import (scheme base)
        (scheme load)
        (scheme write))

(load "common.scm")


(print
  (call/cc (lambda (k)
    (with-exception-handler
      (lambda (err)
        (display "catch: ")
        (write (error-object-message err))
        (newline)
        (k (error-object-irritants err)))
      (lambda ()
        (error "testing exception handling" 'ok))))))


(define-record-type <pare> ;; record name
  (kons x y)               ;; constructor
  pare?                    ;; type predicate
  (x kar set-kar!)         ;; accessor and modifier for x
  (y kdr))                 ;; accessor (and no modifier) for y


(define radix
  (make-parameter
    10
    (lambda (x)
      (unless (and (exact-integer? x) (<= 2 x 16))
        (error "invalid radix"))
      x)))

(define (convert num) (number->string num (radix)))

(print (convert 7))
(parameterize ((radix 2))
  (print (convert 7)))


(print
  (do ((vec (make-vector 5)) (i 0 (+ i 1)))
      ((= i 5) vec)
    (vector-set! vec i i)))
