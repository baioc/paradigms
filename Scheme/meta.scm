;; ************************ GENERIC INTERPRETER ********************************

; ;; simple dispatch on type
; (define (eval exp env)
;   (cond ; primitives
;         ((self-evaluating? exp) exp)
;         ((variable? exp) (lookup-variable exp env))
;         ; special forms
;         ((quotation? exp) (quoted exp))
;         ((assignment? exp) (eval-assignment exp env))
;         ((definition? exp) (eval-definition exp env))
;         ((if? exp) (eval-if exp env))
;         ((lambda? exp)
;          (make-procedure (lambda-parameters exp)
;                          (lambda-body exp)
;                          env))
;         ((begin? exp)
;          (eval-sequence (begin-actions exp) env))
;         ((cond? exp) (eval (cond->if exp) env))
;         ; derivate form
;         ((application? exp)
;          (apply (eval (operator exp) env)
;                 (list-of-values (operands exp) env)))
;         ; unknown
;         (else (error "Unknown expression type -- EVAL" exp))))

;; data-directed style dispatch
(define (eval exp env)
  (cond ; primitives
        ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable exp env))
        ; combinations
        ((reserved? exp) (eval-rules exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ; unknown
        (else (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ; goto primitive routine
        ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ; or evaluate a combination
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        ; fail
        (else (error "Unknown procedure type -- APPLY" procedure))))


;; ************************** EVALUATION RULES *********************************

(define reserved-word-rules
  (list
    ('quote
      (lambda (exp env) (quoted exp)))
    ('set!
      eval-assignment)
    ('lambda
      (lambda (exp env)
        (make-procedure (lambda-parameters exp)
                        (lambda-body exp)
                        env)))
    ('define
      eval-definition)
    ('if
      eval-if)
    ('begin
      (lambda (exp env) (eval-sequence (begin-actions exp) env)))
    ('cond
      (lambda (exp env) (eval (cond->if exp) env)))))

(define (eval-rules exp env)
  ((assq (car exp) reserved-word-rules) exp env))

;; @NOTE: this could be removed by replacing the consequent of application? for
;; (map (eval (operator exp) env) (operands exp)), but that would imply this
;; evaluator depends on language support for higher-order procedures
(define (list-of-values exps env)
  (if (no-operands? exps) '()
      ;; @XXX: order of evaluation of a list of expressions is the same as cons'
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; @NOTE: the procedure true? is used here because the truth value of an
;; expression may differ between the evaluator's and the implemented languages
(define (true? predicate) predicate)
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; @FIXME: this implementation of define ignores a subtle issue in the handling
;; of internal definitions, although it works correctly in most cases.
;; We will see what the problem is and how to solve it in section 4.1.6.
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; @NOTE: returning 'ok is optional and implementation dependent
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)


;; *********************** SYMBOLIC REPRESENTATION *****************************

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (reserved? exp)
  (if (pair? exp)
      (assq (car exp) reserved-words) ; assq uses eq?, better for symbol compare
      false))

; (define (tagged-list? exp tag)
;   (if (pair? exp)
;       (eq? (car exp) tag)
;       false))


; (define (quotation? exp)
;   (tagged-list? exp 'quote))

(define (quoted exp)
  (cadr exp))


; (define (assignment? exp)
;   (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))


; (define (lambda? exp)
;   (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


; (define (definition? exp)
;   (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)    ; direct definition
      (caadr exp))) ; procedure definition

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; parameters
                   (cddr exp)))) ; body


; (define (if? exp)
;   (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

;; @NOTE: the value of an if expression when the predicate is false and there is
;; no alternative is unspecified in Scheme; we have chosen here to make it false
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


; (define (begin? exp)
;   (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (make-begin seq)
  (cons 'begin seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))


; (define (cond? exp)
;   (tagged-list? exp 'cond))

(define (cond-clauses exp)
  (cdr exp))

(define (cond-predicate clause)
  (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses) 'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;; @NOTE: differently than the special forms, the derivate combination isn't
;; tagged thus it must be checked last in eval's cond expression type dispatch
(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))
