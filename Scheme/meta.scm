;; ************************ GENERIC INTERPRETER ********************************

;; data-directed style dispatch
(define (evaln exp env)
  (cond ; primitives
        ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable exp env))
        ; combinations
        ((reserved? exp) (eval-rules exp env))
        ((application? exp)
         (applyn (evaln (operator exp) env)
                 (list-of-values (operands exp) env)))
        ; unknown
        (else (error "Unknown expression type -- EVAL" exp))))

(define apply-in-underlying-scheme apply) ;; save scheme
(define (applyn procedure arguments)
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

(define (list-of-values exps env)
  (if (no-operands? exps) '()
      ;; @NOTE: this sets the order of evaluation of a list of expressions
      (let* (
             (right (list-of-values (rest-operands exps) env))
             (left (evaln (first-operand exps) env))
            )
        (cons left right))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (evaln (first-exp exps) env))
        (else (evaln (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


;; @NOTE: the procedure true? is used here because the truth value of an
;; expression may differ between the evaluator's and the implemented languages
(define (true? predicate)
  (not (false? predicate)))

(define (false? predicate)
  (eq? predicate '#f))

(define (eval-if exp env)
  (if (true? (evaln (if-predicate exp) env))
      (evaln (if-consequent exp) env)
      (evaln (if-alternative exp) env)))


(define (eval-and clauses env)
  (if (empty-exp? clauses) 'true
      (let ((result (evaln (first-exp clauses) env)))
        (cond ((false? result) 'false)
              ((last-exp? clauses) result)
              (else (eval-and (rest-exps clauses) env))))))

(define (eval-or clauses env)
  (if (empty-exp? clauses) 'false
      (let ((result (evaln (first-exp clauses) env)))
        (cond ((true? result) result)
              (else (eval-or (rest-exps clauses) env))))))


;; @FIXME: this implementation of define ignores a subtle issue in the handling
;; of internal definitions, although it works correctly in most cases.
;; We will see what the problem is and how to solve it in section 4.1.6.
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (evaln (definition-value exp) env)
                    env)
  'ok)

;; @NOTE: returning 'ok is optional and implementation dependent
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (evaln (assignment-value exp) env)
                       env)
  'ok)


(define reserved-word-rules
  (list
    (cons 'quote
      (lambda (exp env) (quoted exp)))
    (cons 'set!
      (lambda (exp env) (eval-assignment exp env)))
    (cons 'lambda
      (lambda (exp env)
        (make-procedure (lambda-parameters exp)
                        (lambda-body exp)
                        env)))
    (cons 'define
      (lambda (exp env) (eval-definition exp env)))
    (cons 'if
      (lambda (exp env) (eval-if exp env)))
    (cons 'begin
      (lambda (exp env) (eval-sequence (begin-actions exp) env)))
    (cons 'cond
      (lambda (exp env) (evaln (cond->if exp) env)))
    (cons 'and
      (lambda (exp env) (eval-and (and-clauses exp) env)))
    (cons 'or
      (lambda (exp env) (eval-or (or-clauses exp) env)))
    (cons 'let
      (lambda (exp env) (evaln (let->combination exp) env)))
    (cons 'let*
      (lambda (exp env) (evaln (let*->let exp) env)))))

(define (eval-rules exp env)
  ((cdr (assq (car exp) reserved-word-rules)) exp env))


;; *********************** SYMBOLIC REPRESENTATION *****************************

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp)
  (symbol? exp))

(define (reserved? exp)
  (if (and (pair? exp)
           (assq (car exp) reserved-word-rules))
      #t
      #f))

(define (tagged-list? exp tag)
  (if (and (pair? exp)
           (eq? (car exp) tag))
      #t
      #f))


(define (quoted exp)
  (cadr exp))


(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))


(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)    ; direct definition
      (caadr exp))) ; procedure definition

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; parameters
                   (cddr exp)))) ; body


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


(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (empty-exp? seq)
  (null? seq))

(define (last-exp? seq)
  (empty-exp? (rest-exps seq)))


(define (begin-actions exp)
  (cdr exp))

(define (make-begin seq)
  (cons 'begin seq))

(define (sequence->exp seq)
  (cond ((empty-exp? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))


(define (and-clauses exp)
  (cdr exp))

(define (or-clauses exp)
  (cdr exp))


(define (cond-clauses exp)
  (cdr exp))

(define (cond-predicate clause)
  (car clause))

;; Scheme allows an additional syntax for cond clauses, (<test> => <recipient>).
;; If <test> evaluates to a true value, then <recipient> is evaluated: its value
;; must be a procedure of one argument; this procedure is then invoked on <test>,
;; and the result is returned as the value of the cond expression. For example:
;; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;       (else #f)) -> 2
(define (cond-actions clause)
  (if (eq? (cadr clause) '=>)
      (list (list (caddr clause) (cond-predicate clause)))
      (cdr clause)))

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


(define (let-body exp)
  (caddr exp))

(define (let-associations exp)
  (cadr exp))

(define (let-vars exp)
  (map car (let-associations exp)))

(define (let-inits exp)
  (map cadr (let-associations exp)))

; (let ((<var1> <exp1>)          <=>        ((lambda (<var1> ... <varn>)
;         ...                                  <body>)
;       (<varn> <expn>))                     <exp1>
;   <body>)                                   ...
;                                            <expn>)
(define (let->combination exp)
  (cons (make-lambda (let-vars exp)
                     (let-body exp))
        (let-inits exp)))

(define (make-let associations body)
  (list 'let associations body))

(define (let-reduce associations body)
  (if (null? associations)
      body
      (make-let (list (car associations))
                (let-reduce (cdr associations) body))))

(define (let*->let exp)
  (let-reduce (let-associations exp) (let-body exp)))


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


(define (compound-procedure? exp)
  (tagged-list? exp 'procedure))

(define (procedure-parameters proc)
  (cadr proc))

(define (procedure-body proc)
  (caddr proc))

(define (procedure-environment proc)
  (cadddr proc))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))


;; ****************************** ENVIRONMENT **********************************

(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define empty-environment
  '())


(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) ; variable not bound locally? check outer scope
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (cons 'not not)
        (cons '+ +)
        (cons '- -)
        (cons '= =)
        (cons '< <)
        (cons '> >)
        (cons '* *)
        (cons '/ /)
        (cons 'remainder remainder)
        (cons 'modulo modulo)
        (cons 'abs abs)
        (cons 'eq? eq?)
        (cons 'equal? equal?)
        (cons 'eqv? eqv?)
        (cons 'number? number?)
        (cons 'string? string?)
        (cons 'symbol? symbol?)
        (cons 'pair? pair?)
        (cons 'read read)
        (cons 'display display)
        (cons 'exit exit)
        ;
        (cons 'eval evaln)
        (cons 'apply applyn)
        ;
        (cons 'cons cons)
        (cons 'car car)
        (cons 'cdr cdr)
        (cons 'null? null?)
        (cons 'list list)
        (cons 'list? list?)
        (cons 'append append)
        (cons 'length length)
        (cons 'assoc assoc)
        (cons 'assq assq)
        (cons 'assv assv)
        ;
        (cons 'cadr cadr)
        (cons 'caddr caddr)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cdr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))


;; ************************ Read-Eval-Print-Loop *******************************

(define global-environment (setup-environment))

(define input-prompt ">> ")

(define output-prompt "")

(define (repl)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (evaln input global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (repl))

(define (prompt-for-input string)
  (newline) (display string))

(define (announce-output string)
  (display string))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(display "ABRACADABRA (version L-0.1)\n")

(repl)
