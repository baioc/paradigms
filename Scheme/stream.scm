;; memo-proc'ed no-args procedure is only evaluated once, result is cached
(define (memo-proc p)
  (let ((run? #f) (result '()))
    (lambda ()
      (if run?
          result
          (let ((y (p)))
            (begin (set! run? #t)
                   (set! result y)
                   y))))))

;; special define syntax in order to perform delayed evaluation of arguments
(define-syntax lazy
  (syntax-rules ()
    ((lazy exp)
      (memo-proc (lambda () exp)))))

(define (sforce p)
  (p))

;; streams as "on-demand" data structures
(define-syntax stream
  (syntax-rules ()
    ((stream x y)
      (cons x (lazy y)))))

(define (head s)
  (car s))

(define (tail s)
  (sforce (cdr s)))

(define (empty? s)
  (null? s))

(define empty
  '())

(define (smap-unary proc s)
  (if (empty? s) s
      (stream (proc (head s))
              (smap-unary proc (tail s)))))

;; a more general map takes a procedure of n arguments, together with n lists,
;; and applies the procedure to all the first elements of the lists, all the
;; second elements of the lists, and so on, returning a list of the results. eg:
;; (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))  ->  (741 852 963)
(define (smap proc . streamlist)
  (if (or (null? streamlist)
          (empty? (car streamlist)))
      empty
      (stream
        (apply proc (map head streamlist))
        (apply smap
               (cons proc (map tail streamlist))))))

(define (filter pred s)
  (cond ((empty? s) s)
        ((pred (head s))
           (stream (head s)
                   (filter pred (tail s))))
        (else (filter pred (tail s)))))

(define (accumulate op init s)
  (if (empty? s) init
      (op (head s)
          (accumulate op init (tail s)))))

(define (sappend s1 s2)
  (if (empty? s1) s2
      (stream (head s1)
              (sappend (tail s1) s2))))

(define (range lo hi)
  (if (> lo hi) empty
      (stream lo (range (+ lo 1) hi))))

(define (foreach proc s)
  (cond ((not (empty? s))
           (proc (head s))
           (foreach proc (tail s)))))

(define (nth n s)
  (if (= n 0) (head s)
      (nth (- n 1) (tail s))))


(define (ints-from n)
  (stream n (ints-from (+ n 1))))

;; Sieve of Eratosthenes
(define (sieve ns)
  (define (divisible? a b)
    (= 0 (remainder a b)))
  (stream (head ns)
          (sieve (filter (lambda (x) (not (divisible? x (head ns))))
                         (tail ns)))))

(define primes
  (sieve (ints-from 2)))


(define (sqrt-stream x)
  (define (average a b)
    (/ (+ a b) 2))

  (define (improve guess x)
    (average guess (/ x guess)))

  (stream 1.0
          (smap (lambda (guess)
                  (improve guess x))
                (sqrt-stream x))))

(define (partial-sums s)
  (smap + s (stream 0 (partial-sums s))))

;; Madhava's formula: the Gregory–Leibniz series for arctan(z=1)
(define (pi-summands n)
  (stream (/ 1.0 n)
          (smap - (pi-summands (+ n 2)))))

(define pi-stream
  ;; stream the successive approximations of pi through the series
  (smap (lambda (x) (* x 4))
        (partial-sums (pi-summands 1))))

;; Leonhard Euler's transform is an accelerator (converts a sequence of approximations
;; to one that converges faster) that works well for partial sums with alternating signs
(define (euler-transform seq)
  (define (square x) (* x x))
  (let ((Sn-1 (nth 0 seq))
        (Sn (nth 1 seq))
        (Sn+1 (nth 2 seq)))
    (let ((En (- Sn+1 (/ (square (- Sn+1 Sn))
                         (+ Sn-1 (* -2 Sn) Sn+1)))))
      (stream (if (nan? En) Sn+1 En)
              (euler-transform (tail seq))))))

;; a stream of successively transformed streams
(define (tableau t s)
  (stream s
          (tableau t (t s))))

(define (accelerated transform seq)
  (smap head (tableau transform seq)))
