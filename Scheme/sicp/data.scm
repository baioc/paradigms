;; make a "monitored" procedure which tracks how many times it was called
(define (make-monitored proc)
  (define (count n)
    (define (delegate . args)
      (cond ((and (not (null? args))
                  (eq? (car args) 'how-many-calls?))
             n)
            (else (begin (set! n (+ n 1))
                         (apply proc args)))))
    delegate)
  (count 0))


;; literally a dictionary/map
(define (make-table)
  (cons '*table* '()))

(define (lookup table key)
  (let ((record (assoc key (cdr table))))
    (cond (record => cdr)
          (else #f))))

(define (insert! table key value)
  (cond ((assoc key (cdr table)) => (lambda (record) (set-cdr! record value)))
        (else (set-cdr! table
                        (cons (cons key value) (cdr table)))))
  'ok)

(define (clear-table! table)
  (set-cdr! table '())
  'ok)

;; Guile only
; (define make-table make-hash-table)
; (define lookup hash-ref)
; (define insert! hash-set!)


;; make a function that caches its past results
(define (memoize f)
  (let ((cache (make-table)))
    (define (delegate x)
      (let ((hit (lookup cache x)))
        (or hit
            (let ((y (f x)))
              (insert! cache x y)
              y))))
    delegate))


(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))


;; pointers to head and tail
(define (make-queue)
  (cons '() '()))

(define (empty-queue? queue)
  (null? (car queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Queue is empty" queue)
      (caar queue)))

(define (enqueue! queue item)
  (let ((node (cons item '())))
    (cond ((empty-queue? queue)
           (set-car! queue node)
           (set-cdr! queue node))
          (else
           (set-cdr! (cdr queue) node)
           (set-cdr! queue node)))))

(define (dequeue! queue)
  (cond ((empty-queue? queue)
         (error "Queue underflow" queue))
        (else
         (set-car! queue (cdar queue)))))
