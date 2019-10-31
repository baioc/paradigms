;;; Implementing 2D Guile arrays (matrices) using standard vectors

(define (make-array v m n)
  (let build-rows! ((mat (make-vector m)) (i 0))
    (if (= i m) mat
        (begin
          (vector-set! mat i (make-vector n v))
          (build-rows! mat (+ i 1))))))

(define (array-dimensions mat) ; -> (m n)
  (list (vector-length mat) (vector-length (vector-ref mat 0))))

(define (array-ref mat i j)
  (vector-ref (vector-ref mat i) j))

(define (array-set! mat x i j)
  (vector-set! (vector-ref mat i) j x))

(define (array-map! new proc mat)
  (let* ((dim (array-dimensions mat))
         (m (car dim)) (n (cadr dim)))
    (let iter ((i 0) (j 0))
      (if (< i m)
          (if (= j n) (iter (+ i 1) 0)
            (begin
              (array-set! new (proc (array-ref mat i j)) i j)
              (iter i (+ j 1))))))))

(define (array->list mat)
  (vector->list (vector-map vector->list mat)))

(define (list->array bounds lst) ;; @XXX: bounds are ignored
  (let build-rows! ((vec (make-vector (length lst))) (idx 0) (lst lst))
    (if (null? lst) vec
        (begin
          (vector-set! vec idx (list->vector (car lst)))
          (build-rows! vec (+ idx 1) (cdr lst))))))
