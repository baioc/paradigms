(define-syntax assert
  (syntax-rules ()
    [(_ expr)
     (when (not expr)
       (error 'r6rs-error "failed assertion" '(assert expr)))]))


(define (probability? x)
  (and (number? x) (<= 0 x 1)))

(define (bit probability-of-1)
  (assert (probability? probability-of-1))
  probability-of-1)

(define (b/not x)
  (bit (- 1 x)))

(define (b/or x y)
  (bit (max x y)))

(define (b/and x y)
  (bit (min x y)))


(define (xor x y)
  (b/or
    (b/and (b/not x) y)
    (b/and x (b/not y))))

(define (full-adder-1b a b carry-in)
  (let* ([a-xor-b (xor a b)]
         [a-and-b (b/and a b)]
         [sum (xor a-xor-b carry-in)]
         [carry-out (b/or a-and-b (b/and a-xor-b carry-in))])
    (vector sum carry-out)))


(define (word . args)
  (map bit args))

(define (adder A B)
  (define (ripple-carry-adder a A b B)
    (let* ([lower-bits (if (or (null? A) (null? B)) (word 0) (adder A B))]
           [carry-in (car lower-bits)]
           [lower-sum (cdr lower-bits)]
           [full-add (full-adder-1b a b carry-in)]
           [sum (vector-ref full-add 0)]
           [carry-out (vector-ref full-add 1)])
         (cons carry-out
               (cons sum lower-sum))))
  (begin
    (assert (not (null? A)))
    (assert (= (length A) (length B)))
    (ripple-carry-adder
      (car A) (cdr A)
      (car B) (cdr B))))

(define (multiplier A B)
  (define n (length A))
  (define (partial-product-with-A b msb-offset)
    (let ([partial-product (map (lambda (a) (b/and a b)) A)]
          [suffix (make-list (- n 1 msb-offset) (bit 0))])
      (append partial-product suffix)))
  (begin
    (assert (not (null? A)))
    (assert (= (length A) (length B)))
    (let* ([msb-offsets (iota n)]
           [partial-products (map partial-product-with-A B msb-offsets)]
           [zeroes (make-list n (bit 0))])
      (fold-right adder zeroes partial-products))))
