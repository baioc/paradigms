;; slow exponentiation O(2^(n-1)) for base two
;; 2^n = 2 * 2^(n-1) = 2^(n-1) + 2^(n-1)
(define (2^ n)
  (cond ((= n 0) 1)
        ((= n 1) 2)
        (else (+ (2^ (- n 1))
                 (2^ (- n 1))))))

;; recursive O(lg(n)) version, via successive squaring
;; b^n = (b^(n/2))^2
(define (^ b n)
  (cond ((= n 0) 1)
        ((even? n) (square (^ b (halve n))))
        (else (* b (^ b (- n 1))))))

;; iterative successive squaring
;; b^n = (b^2)^(n/2)
(define (^ b n)
  (define (iter b n prod)
    (cond ((= n 0) prod)
          ((even? n) (iter (square b) (halve n) prod))
          (else (iter b (- n 1) (* b prod)))))
  (if (< n 0)
      (iter (/ 1 b) (- n) 1)
      (iter b n 1)))


;; recursive O(n) multiplication
(define (times a n)
  (if (= n 0) 0
      (+ a (times a (- n 1)))))

;; iterative O(n) multiplication
(define (times a n)
  (define (iter a n prod)
    ; (display a) (display n) (display prod) (newline)
    (if (= n 0) prod
        (iter a (- n 1) (+ prod a))))
  (if (< n 0) (iter (- a) (- n) 0)
      (iter a n 0)))

;; Russian peasant multiplication, O(lg(n))
(define (times a n)
  (define (iter a n prod)
    ; (display a) (display n) (display prod) (newline)
    (cond ((= n 0) prod)
          ((even? n) (iter (double a) (halve n) prod))
          (else (iter a (- n 1) (+ prod a)))))
  (if (< n 0)
      (iter (- a) (- n) 0)
      (iter a n 0)))


(define (pow power operation basis neutral succession)
  (let iter ((b basis) (n power) (acc neutral))
    (cond ((= n 0) acc)
          ((even? n) (iter (succession b) (halve n) acc))
          (else (iter b (- n 1) (operation b acc))))))

(define (empower operation neutral . opts-succ)
  (let ((succession (maybe-car opts-succ (lambda (x) (operation x x)))))
    (lambda (basis power)
      (pow power operation basis neutral succession))))


(define times (empower + 0))

(define (mul b n)
  (if (< n 0)
      (times (- b) (- n))
      (times b n)))

(define (^ b n)
  (let ((raise (empower * 1)))
    (if (< n 0)
        (raise (/ 1 b) (- n))
        (raise b n))))

(define (fibonacci n)
  (let ((fn (cadr (pow ;; nth power
                       (abs n)
                       ;; of the binary operation / transformation
                       (lambda (coefs fibs)
                         (let ((p (car coefs)) (q (cadr coefs))
                               (a (car fibs)) (b (cadr fibs)))
                           (list (+ (* b q) (* a (+ q p)))
                                 (+ (* b p) (* a q)))))
                       ;; starting from basis
                       '(0 1)
                       ;; accumulated over
                       '(1 0)
                       ;; where the successive squaring that empowers
                       ;; the operation is given by
                       (lambda (coefs)
                         (let ((p (car coefs)) (q (cadr coefs)))
                           (list (+ (square q) (square p))
                                 (+ (square q) (* 2 (* p q))))))))))
    ;; negafibonacci: F(-n) = Fn * (-1)^(n+1))
    (if (and (< n 0)
             (even? n))
        (- fn)
        fn)))

;; matrix fibonacci
(define (fibona n) ;; n > 0
  (define (mul-matrix-2x2 A B)
    (let ((a11 (caar A)) (a12 (cadar A))
          (a21 (caadr A)) (a22 (cadadr A))
          (b11 (caar B)) (b12 (cadar B))
          (b21 (caadr B)) (b22 (cadadr B)))
      (list (list (+ (* a11 b11) (* a12 b21))
                  (+ (* a11 b12) (* a12 b22)))
            (list (+ (* a21 b11) (* a22 b21))
                  (+ (* a21 b12) (* a22 b22))))))
  (let ((nth-transform (empower mul-matrix-2x2
                                '((1 0)
                                  (0 1)))))
    (caar
      (nth-transform '((1 1)
                       (1 0))
                     (- n 1)))))

;; closed form through eigen-stuff
(define phi (/ (+ 1 (sqrt 5)) 2)) ;; golden ration
(define fi (- 1 phi)) ;; golden ratio complement
(define (fibonac n)
  (round (/ (- (^ phi n) (^ fi n))
            (sqrt 5))))
