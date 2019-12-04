;; these are used to make different labels from a given name to avoid conflicts
(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))


;; code: statements which depend on some values and modify other values
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))


;; compiles an expression into object code such that the expression's value
;; is put into target register and execution follows from linkage descriptor
(define (compile expr target linkage)
  (cond ((self-evaluating? expr)
         (compile-self-evaluating expr target linkage))
        ((quoted? expr) (compile-quoted expr target linkage))
        ((variable? expr)
         (compile-variable expr target linkage))
        ((assignment? expr)
         (compile-assignment expr target linkage))
        ((definition? expr)
         (compile-definition expr target linkage))
        ((if? expr) (compile-if expr target linkage))
        ((lambda? expr) (compile-lambda expr target linkage))
        ((begin? expr)
         (compile-sequence (begin-actions expr) target linkage))
        ((cond? expr) (compile (cond->if (clauses expr)) target linkage))
        ((application? expr)
         (compile-application expr target linkage))
        (else
         (error "Unknown expression type -- COMPILE" expr))))


(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

(define (compile-self-evaluating expr target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence '() (list target)
      `((assign ,target (const ,expr))))))

(define (compile-quoted expr target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence '() (list target)
      `((assign ,target (const ,(text-of-quotation expr)))))))

(define (compile-variable expr target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence '(env) (list target)
      `((assign ,target
                (op lookup-variable-value)
                (const ,expr)
                (reg env))))))


(define (compile-assignment expr target linkage)
  (let ((var (assignment-variable expr))
        (get-value-code (compile (assignment-value expr) 'val 'next)))
    (end-with-linkage linkage
      (preserving '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
          `((perform (op set-variable-value!)
                     (const ,var)
                     (reg val)
                     (reg env))
            (assign ,target (const ok))))))))

(define (compile-definition expr target linkage)
  (let ((var (definition-variable expr))
        (get-value-code (compile (definition-value expr) 'val 'next)))
    (end-with-linkage linkage
      (preserving '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
          `((perform (op define-variable!)
                     (const ,var)
                     (reg val)
                     (reg env))
            (assign ,target (const ok))))))))


(define (compile-if expr target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate expr) 'val 'next))
            (c-code (compile (if-consequent expr) target consequent-linkage))
            (a-code (compile (if-alternative expr) target linkage)))
        (preserving '(env continue)
          p-code
          (append-instruction-sequences
            (make-instruction-sequence '(val) '()
             `((test (op false?) (reg val))
               (branch (label ,f-branch))))
            (parallel-instruction-sequences
             (append-instruction-sequences t-branch c-code)
             (append-instruction-sequences f-branch a-code))
            after-if))))))

(define (cond->if clauses)
  (if (null? clauses) 'false ;; no else clause
      (let ((curr (car clauses))
            (rest (cdr clauses)))
        (cond ((eq? (car curr) 'else)
                (if (not (null? (cdr clauses)))
                    (display "Warning: ELSE clause isn't last"))
                (cons 'begin (cdr curr)))
              (else (list 'if
                          (car curr)
                          (cons 'begin (cdr curr))
                          (cond->if rest)))))))
