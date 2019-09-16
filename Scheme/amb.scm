;; reference: https://www.scheme.com/tspl3/syntax.html

(define-syntax when
  (syntax-rules ()
    ((_ test branch ...) (if test (begin branch ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ test usual exception) (if (not test) usual exception))))


(define amb-fail 'uninitialized)

(define (amb-init)
  (set! amb-fail (lambda ()
                   (error ";;; possibility space exhausted")
                   (amb-init))))

(amb-init)

(define-syntax amb
  (syntax-rules ()
    ((_ branches)
      (let ((backtrack amb-fail))
        (call/cc (lambda (accept)
                   (map (lambda (alternative)
                          (call/cc (lambda (keep-trying)
                                     (set! amb-fail (lambda ()
                                                      (set! amb-fail backtrack)
                                                      (keep-trying 'failed)))
                                     (accept alternative))))
                        branches)
                   (backtrack)))))))

(define (amb-retry)
  (amb '()))

(define (amb-req pred)
  (if (not pred) (amb-retry)))


(define (square x)
  (* x x))

(define (range lo hi)
  (if (> lo hi) '()
      (cons lo (range (+ lo 1) hi))))

(define (amb-pythagorean max)
  (let ((a (amb (range 1 max)))
        (b (amb (range 1 max)))
        (c (amb (range 1 max))))
    (amb-req (pythagorean? a b c))
    (list a b c)))

(define (pythagorean? a b c)
  (and (triangular? a b c)
       (= (square c) (+ (square a) (square b)))))

(define (triangular? a b c)
  (and (<= c (+ a b))
       (<= b (+ a c))
       (<= a (+ b c))))


(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

(define (liars)
  ;; Five schoolgirls sat for an examination.
  (let ((betty (amb (range 1 5)))
        (ethel (amb (range 1 5)))
        (joan (amb (range 1 5)))
        (kitty (amb (range 1 5)))
        (mary (amb (range 1 5))))
    ;; They agreed that, in writing home about the examination, each should
    ;; make one true statement and one untrue one.
    ;; The following are the relevant passages from their letters:
    ;; Betty: "Kitty was second in the examination. I was only third."
    (amb-req (or (and (= kitty 2)
                      (not (= betty 3)))
                 (and (not (= kitty 2))
                      (= betty 3))))
    ;; Ethel: "You'll be glad to hear that I was on top. Joan was second."
    (amb-req (or (and (= ethel 1)
                      (not (= joan 2)))
                 (and (not (= ethel 1))
                      (= joan 2))))
    ;; Joan: "I was third, and poor old Ethel was bottom."
    (amb-req (or (and (= joan 3)
                      (not (= ethel 5)))
                 (and (not (= joan 3))
                      (= ethel 5))))
    ;; Kitty: "I came out second. Mary was only fourth."
    (amb-req (or (and (= kitty 2)
                      (not (= mary 4)))
                 (and (not (= kitty 2))
                      (= mary 4))))
    ;; Mary: "I was fourth. Top place was taken by Betty."
    (amb-req (or (and (= mary 4)
                      (not (= betty 1)))
                 (and (not (= mary 4))
                      (= betty 1))))
    (amb-req (distinct? (list betty ethel joan kitty mary)))
    (list betty ethel joan kitty mary)))
