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

;; (assoc 'x '((a 1) (b 2) (x 3) (c 4))) -> '(x 3) : #f, uses equal?
(define (lookup table key)
  (let ((record (assoc key (cdr table))))
    (cond (record => cdr)
          (else #f))))

(define (insert! table key value)
  (cond ((assoc key (cdr table)) => (lambda (record) (set-cdr! record value)))
        (else (set-cdr! table
                        (cons (cons key value) (cdr table)))))
  'ok)

;; Guile only
(define make-table make-hash-table)
(define lookup hash-ref)
(define insert! hash-set!)

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


(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? self))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? self)
          (inform-about-value new-constraint))
      'done)
    (define (self request)
      (cond ((eq? request 'has-value?) informant)
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
    self))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else
           (procedure (car items))
           (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       self))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       self))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       self))))
  (define (process-forget-value)
    (forget-value! sum self)
    (forget-value! a1 self)
    (forget-value! a2 self)
    (process-new-value))
  (define (self request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 self)
  (connect a2 self)
  (connect sum self)
  self)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 self))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       self))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       self))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       self))))
  (define (process-forget-value)
    (forget-value! product self)
    (forget-value! m1 self)
    (forget-value! m2 self)
    (process-new-value))
  (define (self request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 self)
  (connect m2 self)
  (connect product self)
  self)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (constant value connector)
  (define (self request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector self)
  (set-value! connector value self)
  self)

(define (probe name connector)
  (define (print-probe value)
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (self request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector self)
  self)

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (cv val)
  (let ((c (make-connector)))
    (constant val c)
    c))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))


(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

; (define (celsius-fahrenheit-converter c f)
;   (let ((u (make-connector))
;         (v (make-connector))
;         (w (make-connector))
;         (x (make-connector))
;         (y (make-connector)))
;     (constant 9 w)
;     (constant 5 x)
;     (constant 32 y)
;     (multiplier c w u)
;     (multiplier v x u)
;     (adder v y f)
;     'ok))
; (define C (make-connector))
; (define F (make-connector))
; (celsius-fahrenheit-converter C F)

(probe "Celsius" C)
(probe "Fahrenheit" F)

; (set-value! c 0 'user)
; (set-value! f 212 'user)
; (forget-value! c 'user)
; (set-value! f 212 'user)


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
