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


;; pointers to head and tail
(define (make-queue)
  (cons '() '()))

(define (empty-queue? queue)
  (null? (car queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
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
         (error "DEQUEUE! called with an empty queue" queue))
        (else
         (set-car! queue (cdar queue)))))


;; literally a dictionary/map
(define (make-table)
  (cons '*table* '()))

;; (assv 'x '((a 1) (b 2) (x 3) (c 4))) -> '(x 3) : #f, uses eqv? for comparison
(define (lookup table key)
  (let ((record (assv key (cdr table))))
    (cond (record => cdr)
          (else #f))))

(define (insert! table key value)
  (let ((record (assv key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)


;; make a function that caches its past results
(define (memoize func)
  (let ((results (make-table)))
    (define (delegate . args)
      (let ((cached (lookup results args)))
        (or cached
            (let ((y (apply func args)))
              (insert! results args y)
              y))))
    delegate))
