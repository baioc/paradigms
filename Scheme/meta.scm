;; ******************************** LOGGER *************************************
(define (debug-log . msgs) (logger 4 'DEBUG ": " msgs))
(define (info-log . msgs) (logger 3 'INFO ": " msgs))
(define (warn-log . msgs) (logger 2 'WARN ": " msgs))
(define (error-log . msgs) (logger 1 'ERROR ": " msgs))

(define (logger level . msgs)
  (if (<= level *log-level*)
      (begin
        (for-each display msgs)
        (newline)
        (car msgs))))

(define *log-level* 3)

(define (set-max-log-level! level)
  (cond ((eq? level 'DEBUG) (set! *log-level* 4))
        ((eq? level 'INFO) (set! *log-level* 3))
        ((eq? level 'WARN) (set! *log-level* 2))
        ((eq? level 'ERROR) (set! *log-level* 1))
        ((eq? level 'QUIET) (set! *log-level* 0))))


;; ************************ GENERIC INTERPRETER ********************************

;; data-directed style dispatch
(define (evaln exp env)
  (debug-log "EVAL" exp)
  (cond ; primitives
        ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable exp env))
        ; combinations
        ((reserved? exp) (eval-rules exp env))
        ((application? exp)
         (begin (debug-log "APPLICATION" exp)
         (applyn (evaln (operator exp) env)
                 (eval-list (operands exp) env))))
        ; unknown
        (else (error-log "Unknown expression type -- EVAL" exp))))

(define apply-in-underlying-scheme apply) ;; save scheme
(define (applyn procedure arguments)
  (cond ; goto primitive routine
        ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ; or evaluate a combination
        ((compound-procedure? procedure)
         (begin (debug-log "APPLY-COMPOUND"
                           (procedure-parameters procedure)
                           (procedure-body procedure)
                           arguments)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure)))))
        ; fail
        (else (error-log "Unknown procedure type -- APPLY" procedure))))


;; ************************** EVALUATION RULES *********************************

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

(define (reserved? exp)
  (if (and (pair? exp)
           (assq (car exp) reserved-word-rules))
      #t
      #f))

(define (eval-rules exp env)
  (debug-log "EVAL-RULES" (car exp))
  ((cdr (assq (car exp) reserved-word-rules)) exp env))


(define (eval-list exps env)
  (if (no-operands? exps) '()
      ;; @NOTE: this sets the order of evaluation of a list of expressions
      (let* (
             (right (eval-list (rest-operands exps) env))
             (left (evaln (first-operand exps) env))
            )
        (cons left right))))

(define (eval-sequence exps env)
  (cond ((empty-exp? exps) (evaln exps env))
        ((last-exp? exps) (evaln (first-exp exps) env))
        (else (evaln (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


;; @NOTE: the procedure true? is used here because the truth value of an
;; expression may differ between the evaluator's and the implemented languages
(define (eval-if exp env)
  (if (true? (evaln (if-predicate exp) env))
      (evaln (if-consequent exp) env)
      (evaln (if-alternative exp) env)))

(define (true? predicate)
  (not (false? predicate)))

(define (false? predicate)
  (eq? predicate '#f))


(define (eval-and clauses env)
  (if (empty-clauses? clauses) 'true
      (let ((result (evaln (first-clause clauses) env)))
        (cond ((false? result) 'false)
              ((last-clause? clauses) result)
              (else (eval-and (rest-clauses clauses) env))))))

(define (eval-or clauses env)
  (if (empty-clauses? clauses) 'false
      (let ((result (evaln (first-clause clauses) env)))
        (cond ((true? result) result)
              (else (eval-or (rest-clauses clauses) env))))))


;; @NOTE: returning 'ok is optional and implementation dependent
(define (eval-definition exp env)
  (debug-log "EVAL-DEFINITION" (definition-variable exp) (definition-value exp))
  (define-variable! (definition-variable exp)
                    (evaln (definition-value exp) env)
                    env)
  'ok)

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (evaln (assignment-value exp) env)
                       env)
  'ok)


;; *********************** SYMBOLIC REPRESENTATION *****************************

(define (self-evaluating? exp)
  (or (null? exp)
      (number? exp)
      (string? exp)))

(define (variable? exp)
  (symbol? exp))

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


(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)    ; direct definition
      (caadr exp))) ; procedure definition

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; parameters
                   (cddr exp)))) ; body


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

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


(define (make-begin seq)
  (list 'begin seq))

(define (begin-actions exp)
  (cdr exp))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (empty-exp? seq)
  (null? seq))

(define (last-exp? seq)
  (empty-exp? (rest-exps seq)))

(define (sequence->exp seq)
  (cond ((empty-exp? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))


(define (cond-clauses exp)
  (cdr exp))

(define (cond-predicate clause)
  (car clause))

;; Scheme allows an additional syntax for cond clauses, (<test> => <recipient>).
;; If <test> evaluates to a true value, then <recipient> is evaluated: its value
;; must be a procedure of one argument; this procedure is then invoked on <test>
;; and the result is returned as the value of the cond expression. For example:
;; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;       (else #f)) -> 2
;; @XXX: as it is, <test> gets evaluated twice, once during the predicate
;; evaluation and then to apply <recipient> to it
(define (cond-actions clause)
  (if (eq? (cadr clause) '=>)
      (list (list (caddr clause) (cond-predicate clause)))
      (cdr clause)))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses) 'false ; no else clause
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error-log "ELSE clause isn't last -- COND->IF" clauses))
              (make-if (cond-predicate first)
                       (sequence->exp (cond-actions first))
                       (expand-clauses rest))))))
  (expand-clauses (cond-clauses exp)))


(define (and-clauses exp)
  (cdr exp))

(define (or-clauses exp)
  (cdr exp))

(define (first-clause lst)
  (car lst))

(define (rest-clauses lst)
  (cdr lst))

(define (empty-clauses? clauses)
  (null? clauses))

(define (last-clause? lst)
  (empty-clauses? (rest-clauses lst)))


(define (make-let associations body)
  (list 'let associations body))

(define (let-associations exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (let-vars exp)
  (map car (let-associations exp)))

(define (let-inits exp)
  (map cadr (let-associations exp)))

;; (let ((<var1> <exp1>)         =>          ((lambda (<var1> ... <varn>)
;;         ...                                  <body>)
;;       (<varn> <expn>))                     <exp1>
;;   <body>)                                   ...
;;                                            <expn>)
(define (let->combination exp)
  (cons (make-lambda (let-vars exp)
                     (let-body exp))
        (let-inits exp)))

(define (let*->let exp)
  (define (let-reduce associations body)
    (if (null? associations)
        (sequence->exp body)
        (make-let (list (car associations))
                  (let-reduce (cdr associations) body))))
  (let-reduce (let-associations exp) (let-body exp)))


;; @NOTE: differently than the special forms, the derivate combination isn't
;; tagged thus it must be checked last in eval's cond expression type dispatch
(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

(define (no-operands? ops)
  (null? ops))


(define (make-procedure parameters body env)
  (debug-log "MAKE-PROCEDURE" parameters "->" (scan-out-defines body))
  (list 'procedure parameters (scan-out-defines body) env))

(define (compound-procedure? exp)
  (tagged-list? exp 'procedure))

(define (procedure-parameters proc)
  (cadr proc))

(define (procedure-body proc)
  (caddr proc))

(define (procedure-environment proc)
  (cadddr proc))

;; (lambda <vars>           =>         (lambda <vars>
;;   (define u <e1>)                     (let ((u '*unassigned*)
;;   (define v <e2>)                           (v '*unassigned*))
;;   <e3>)                                 (set! u <e1>)
;;                                         (set! v <e2>)
;;                                         <e3>))
(define (scan-out-defines body)
  (define (name-defs defines)
    (map (lambda (def) (list (definition-variable def)
                             '*unassigned*))
         defines))
  (define (set-defs defines)
    (map (lambda (def) (list 'set!
                             (definition-variable def)
                             (definition-value def)))
         defines))
  (define (defines->let exprs defs ndefs)
    (cond ((null? exprs)
           (if (null? defs) body
               (make-let (name-defs defs)
                         (append (set-defs defs)
                                 ndefs))))
          ((definition? (car exprs))
           (defines->let (cdr exprs)
                         (append defs (list (car exprs)))
                         ndefs))
          (else (defines->let (cdr exprs)
                              defs
                              (append ndefs (list (car exprs)))))))
  (defines->let body '() '()))


;; ****************************** ENVIRONMENT **********************************

(define the-empty-environment
  '())

(define (first-frame env)
  (car env))

(define (enclosing-environment env)
  (cdr env))


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
          (warn-log "Too many arguments supplied" vars vals)
          (warn-log "Too few arguments supplied" vars vals))))

(define (lookup-variable var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) ; variable not bound locally? check outer scope
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error-log "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (warn-log "Unbound variable" var)
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
    (if (eq? env the-empty-environment)
        (warn-log "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))


(define primitive-procedures
  (list
        ;; minimal
        (cons 'not not)
        (cons '+ +)
        (cons '- -)
        (cons '= =)
        (cons '< <)
        (cons '> >)
        ;; reduced
        (cons '<= <=)
        (cons '>= >=)
        (cons '* *)
        (cons '/ /)
        (cons 'remainder remainder)
        (cons 'modulo modulo)
        ;; lisp
        (cons 'equal? equal?)
        (cons 'eqv? eqv?)
        (cons 'eq? eq?)
        (cons 'symbol? symbol?)
        (cons 'pair? pair?)
        (cons 'string? string?)
        (cons 'number? number?)
        (cons 'complex? complex?)
        (cons 'real? real?)
        (cons 'integer? integer?)
        (cons 'rational? rational?)
        (cons 'exact? exact?)
        (cons 'inexact? inexact?)
        (cons 'exit exit)
        ;; io; for more info see https://www.scheme.com/tspl3/io.html
        (cons 'eof-object? eof-object?)
        (cons 'read read)
        (cons 'open-input-file open-input-file)
        (cons 'close-input-port close-input-port)
        (cons 'load load)
        (cons 'newline newline)
        (cons 'display display)
        (cons 'open-output-file open-output-file)
        (cons 'close-output-port close-output-port)
        (cons 'write write)
        (cons 'error error)
        ;; extra
        (cons 'cons cons)
        (cons 'car car)
        (cons 'cdr cdr)
        (cons 'null? null?)
        (cons 'list list)
        (cons 'list? list?)
        (cons 'append append)
        (cons 'length length)
        (cons 'reverse reverse)
        (cons 'abs abs)
        (cons 'even? even?)
        (cons 'odd? odd?)
        (cons 'max max)
        (cons 'min min)
        (cons 'floor floor)
        (cons 'ceiling ceiling)
        (cons 'truncate truncate)
        (cons 'round round)
        (cons 'assoc assoc)
        (cons 'assq assq)
        (cons 'assv assv)
        (cons 'cadr cadr)
        (cons 'caddr caddr)
))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cdr proc)))
       primitive-procedures))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define (apply-primitive-procedure proc args)
  (debug-log "APPLY-PRIMITIVE" proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))


;; ************************ Read-Eval-Print-Loop *******************************

(define global-environment (setup-environment))

(set-max-log-level! 'DEBUG)


(define input-prompt ">> ")

(define (prompt-for-input string)
  (newline) (display string))

(define (announce-output string)
  (display string))

(define (repl)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (eof-object? input)
        (begin
          (newline)
          (info-log "bye!")
          (exit))
        (let ((output (evaln input global-environment)))
          (user-print output))))
  (repl))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


(info-log "ABRACADABRA [version L-0.1]")

(repl)
