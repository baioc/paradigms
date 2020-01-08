;; memo-proc'ed no-args procedure is only evaluated once, result is cached
(define (memo-proc proc)
  (let ((run? #f)
        (cached '()))
    (lambda ()
      (if run? cached
          (let ((result (proc)))
            (begin
              (set! run? #t)
              (set! cached result)
              result))))))

;; macro syntax in order to perform delayed evaluation of arguments
(define-syntax lazy ;; aka delay
  (syntax-rules ()
    ((_ expr) (memo-proc (lambda () expr)))))

;; aka force
(define (thunk p) (p))

;; streams as "on-demand" data structures
(define-syntax stream
  (syntax-rules ()
    ((_ x y) (cons x (lazy y)))))

(define (head s) (car s))
(define (tail s) (thunk (cdr s)))
(define (empty? s) (null? s))
(define empty-stream '())


(define (stream-for-each proc seq)
  (if (not (empty? seq))
      (begin
        (proc (head seq))
        (stream-for-each proc (tail seq)))))

(define (stream-filter pred seq)
  (cond ((empty? seq) seq)
        ((pred (head seq)) (stream (head seq)
                                   (stream-filter pred (tail seq))))
        (else (stream-filter pred (tail seq)))))

(define (stream-foldr op acc seq) ;; aka reduce
  (if (empty? seq) acc
      (op (head seq)
          (stream-foldr op acc (tail seq)))))

(define (stream-zip-with op sa sb)
  (stream (op (head sa) (head sb))
          (stream-zip-with op (tail sa) (tail sb))))

;; a more general map takes a procedure of n arguments, together with n lists,
;; and applies the procedure to all the first elements of the lists, all the
;; second elements of the lists, and so on, returning a list of the results. eg:
;; (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))  ->  (741 852 963)
(define (stream-map proc . streamlist)
  (if (or (null? streamlist)
          (empty? (car streamlist)))
      empty-stream
      (stream
        (apply proc (map head streamlist))
        (apply stream-map
               (cons proc (map tail streamlist))))))

(define (stream-append s1 s2)
  (if (empty? s1) s2
      (stream (head s1)
              (stream-append (tail s1) s2))))

(define (stream-range lo hi)
  (if (> lo hi) empty-stream
      (stream lo (stream-range (+ lo 1) hi))))

(define (stream-ref seq n)
  (cond ((empty? seq) #f)
        ((= n 0) (head seq))
        (else (stream-ref (tail seq) (- n 1)))))


(define (ints-from n)
  (stream n (ints-from (+ n 1))))

;; Sieve of Eratosthenes
(define (sieve seq)
  (define (divisible? a b)
    (= 0 (remainder a b)))
  (if (empty? seq) seq
      (stream (head seq)
              (sieve (stream-filter (lambda (x) (not (divisible? x (head seq))))
                                    (tail seq))))))

(define primes (sieve (ints-from 2)))


(define (arctan-series x n)
  (stream (/ (expt x n) n)
          (stream-map - (arctan-series x (+ n 2)))))

(define (partial-sums s)
  (stream-zip-with + s (stream 0 (partial-sums s))))

;; Madhava's formula: the Gregory-Leibniz series for arctan(z=1)
(define pi-approximations
  ;; stream the successive approximations of pi through the series
  (stream-map (lambda (pi/4) (* 4 pi/4))
              (partial-sums (arctan-series 1.0 1))))


;; Leonhard Euler's transform is an accelerator (converts a sequence of approximations
;; to one that converges faster) that works well for partial sums with alternating signs
(define (euler-transform seq)
  (define (square x) (* x x))
  (define (nan? x) (not (= x x)))
  (let ((Sn-1 (stream-ref seq 0))
        (Sn (stream-ref seq 1))
        (Sn+1 (stream-ref seq 2)))
    (if (or (not (number? Sn-1))
            (not (number? Sn))
            (not (number? Sn+1)))
        empty-stream
        (let ((En (- Sn+1 (/ (square (- Sn+1 Sn))
                             (+ Sn-1 (* -2 Sn) Sn+1)))))
          (if (nan? En)
              (begin (display "Euler Transform converged to machine precision\n")
                     (stream Sn+1 empty-stream))
              (stream En (euler-transform (tail seq))))))))

(define (accelerated transform seq)
  ;; a stream of successively transformed streams
  (define (tableau t s)
    (if (empty? s) s
        (stream s (tableau t (t s)))))
  ;; stream of the first element in each of the tableau's stream
  (stream-map head (tableau transform seq)))


(define (fibonacci-sequence prev curr)
  (stream prev
          (fibonacci-sequence curr (+ prev curr))))

(define fibs (fibonacci-sequence 0 1))


(define (sqrt-approximations x)
  (stream 1.0
          (stream-map (lambda (guess) (average guess (/ x guess)))
                      (sqrt-approximations x))))

;; @TODO Newton's method approximations
