;; memo-proc'ed no-args procedure is only evaluated once, result is cached
(define (memo-proc p)
  (let ((run? #f)
        (result '()))
    (lambda ()
      (if run? result
        (let ((y (p)))
          (begin
            (set! run? #t)
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

(define (smap proc s)
  (if (empty? s) s
      (stream (proc (head s))
              (smap proc (tail s)))))

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
  (stream n
          (ints-from (+ n 1))))

;; Sieve of Eratosthenes
(define (sieve ns)
  (define (divisible? a b)
    (= 0 (modulo a b)))
  (stream (head ns)
          (sieve (filter (lambda (x) (not (divisible? x (head ns))))
                         (tail ns)))))

(define primes
  (sieve (ints-from 2)))
