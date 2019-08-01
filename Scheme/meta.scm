;; ******************************** LOGGER *************************************
(define (debug-log . msgs) (logger 4 (cons "DEBUG:" msgs)))
(define (info-log . msgs) (logger 3 (cons "INFO:" msgs)))
(define (warn-log . msgs) (logger 2 (cons "WARNING:" msgs)))
(define (error-log . msgs) (logger 1 (cons "ERROR:" msgs)))

(define (logger level msgs)
  (if (<= level *log-level*)
      (begin
        (map (lambda (obj) (display obj) (display " ")) msgs)
        (newline))))

(define *log-level* 3)

(define (set-log-verbosity! level)
  (cond ((eq? level 'DEBUG) (set! *log-level* 4))
        ((eq? level 'INFO) (set! *log-level* 3))
        ((eq? level 'WARN) (set! *log-level* 2))
        ((eq? level 'ERROR) (set! *log-level* 1))
        ((eq? level 'QUIET) (set! *log-level* 0))))


;; ************************ GENERIC INTERPRETER ********************************

;; syntactic analysis separated from execution
(define (evaln exp env)
  ((analyze exp) env))

;; data-directed style dispatch
(define (analyze exp)
  (cond ; primitives
        ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ; combinations
        ((reserved? exp) (analyze-syntax-rules exp))
        ((application? exp) (analyze-application exp))
        ; unknown (or null)
        (else (error-log "Unknown expression type -- ANALYZE" exp))))

(define apply-in-underlying-scheme apply) ;; save scheme
(define (execute procedure arguments)
  (cond ; goto primitive routine
        ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ; or evaluate a combination
        ((compound-procedure? procedure)
         ((procedure-body procedure)
          (extend-environment (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure))))
        ; fail
        (else (error-log "Unknown procedure type -- EXECUTE" procedure))))


;; ***************************** SYNTAX RULES **********************************

(define reserved-word-rules
  (list
    (cons 'quote
      (lambda (exp) (analyze-quoted exp)))
    (cons 'set!
      (lambda (exp) (analyze-assignment exp)))
    (cons 'lambda
      (lambda (exp) (analyze-lambda exp)))
    (cons 'define
      (lambda (exp) (analyze-definition exp)))
    (cons 'if
      (lambda (exp) (analyze-if exp)))
    (cons 'begin
      (lambda (exp) (analyze-sequence (begin-actions exp))))
    (cons 'cond
      (lambda (exp) (analyze (cond->if exp))))
    (cons 'and
      (lambda (exp) (analyze-and (and-clauses exp))))
    (cons 'or
      (lambda (exp) (analyze-or (or-clauses exp))))
    ;; @TODO: letrec https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.20
    (cons 'let
      (lambda (exp) (analyze (let->combination exp))))
    (cons 'let*
      (lambda (exp) (analyze (let*->let exp))))))

(define (reserved? exp)
  (and (pair? exp)
       (assq (car exp) reserved-word-rules)))

(define (analyze-syntax-rules exp)
  ((cdr (assq (car exp) reserved-word-rules)) exp))


(define (analyze-sequence exps)
  (define (loop first-proc rest-procs)
    (if (null? rest-procs) first-proc
        (loop (lambda (env) (first-proc env) ((car rest-procs) env))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((funcproc (analyze (operator exp)))
        (argprocs (analyze-list (operands exp))))
    (lambda (env)
      (execute (funcproc env)
               (map (lambda (aproc) (aproc env))
                    argprocs)))))

(define (analyze-list exps)
  (if (no-operands? exps) '()
      ;; @NOTE: this sets the order of evaluation of a list of expressions
      (let* (
             (right (analyze-list (rest-operands exps)))
             (left (analyze (first-operand exps)))
            )
        (cons left right))))


(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (quoted exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable exp env)))

(define (analyze-lambda exp)
  (let ((params (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure params bproc env))))


;; @NOTE: the procedure true? is used here because the truth value of an
;; expression may differ between the evaluator's and the implemented languages
(define (true? predicate)
  (not (false? predicate)))

(define (false? predicate)
  (eq? predicate 'false)) ;; PS: '#f === #f

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))


(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env))))


(define (analyze-and clauses)
  (define (eval-and clauses env)
    (if (empty-clauses? clauses) 'true
        (let ((result (evaln (first-clause clauses) env)))
          (cond ((false? result) 'false)
                ((last-clause? clauses) result)
                (else (eval-and (rest-clauses clauses) env))))))
  (lambda (env) (eval-and clauses env)))

;; @FIXME ^-V

(define (analyze-or clauses)
  (define (eval-or clauses env)
    (if (empty-clauses? clauses) 'false
        (let ((result (evaln (first-clause clauses) env)))
          (cond ((true? result) result)
                (else (eval-or (rest-clauses clauses) env))))))
  (lambda (env) (eval-or clauses env)))


;; *********************** SYMBOLIC REPRESENTATION *****************************

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      ;; @XXX: evaluating () is normally illegal
      (null? exp)
      (char? exp)))

(define (variable? exp)
  (or (symbol? exp)
      (boolean? exp)))

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))


(define (quoted exp)
  (cadr exp))


(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))


(define (make-lambda parameters body)
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
                (list (make-let (name-defs defs)
                                (append (set-defs defs)
                                        (reverse ndefs))))))
            ((definition? (car exprs))
            (defines->let (cdr exprs)
                          (cons (car exprs) defs)
                          ndefs))
            (else (defines->let (cdr exprs)
                                defs
                                (cons (car exprs) ndefs)))))
    (defines->let body '() '()))
  (debug-log "MAKE-LAMBDA" parameters "->" body)
  (cons 'lambda (cons parameters (scan-out-defines body))))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (variable? (cadr exp))
      (cadr exp)    ; direct definition
      (caadr exp))) ; procedure definition

(define (definition-value exp)
  (if (variable? (cadr exp))
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
  (cons 'begin seq))

(define (begin-actions exp)
  (cdr exp))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((null? (cdr seq)) (car seq))
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
;; @FIXME: as it is, <test> gets evaluated twice, once during the predicate
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
  (cons 'let (cons associations body)))

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
                  (list (let-reduce (cdr associations) body)))))
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
  (list 'procedure parameters body env))

(define (compound-procedure? exp)
  (tagged-list? exp 'procedure))

(define (procedure-parameters proc)
  (cadr proc))

(define (procedure-body proc)
  (caddr proc))

(define (procedure-environment proc)
  (cadddr proc))


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
  (set-cdr! frame (cons val (frame-values frame))) 'ok)


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
             (set-car! vals val) 'ok)
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val) 'ok)
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
    (define-variable! 'true 'true initial-env)
    (define-variable! 'false 'false initial-env)
    initial-env))


(define primitive-procedures
  (list ; minimal
        (cons '+ +)
        (cons '- -)
        (cons '= =)
        (cons '< <)
        (cons '> >)
        (cons 'not not)
        ; reduced
        (cons '<= <=)
        (cons '>= >=)
        (cons '* *)
        (cons '/ /)
        (cons '1+ (lambda (n) (+ n 1)))
        (cons '1- (lambda (n) (- n 1)))
        (cons 'zero? zero?)
        (cons 'negative? negative?)
        (cons 'positive? positive?)
        (cons 'quotient quotient)
        (cons 'remainder remainder)
        (cons 'modulo modulo)
        ; Revised^5 Report Scheme ; docs at https://docs.racket-lang.org/guide
        ; symbolic
        (cons 'equal? equal?)
        (cons 'eqv? eqv?)
        (cons 'eq? eq?)
        (cons 'symbol? symbol?)
        (cons 'pair? pair?)
        (cons 'boolean? (lambda (s) (or (eq? s 'true) (eq? s 'false))))
        ; numeric
        (cons 'number? number?)
        (cons 'inexact? inexact?)
        (cons 'exact? exact?)
        (cons 'inexact->exact inexact->exact)
        (cons 'exact->inexact exact->inexact)
        (cons 'complex? complex?)
        (cons 'make-rectangular make-rectangular)
        (cons 'make-polar make-polar)
        (cons 'real-part real-part)
        (cons 'imag-part imag-part)
        (cons 'magnitude magnitude)
        (cons 'angle angle)
        (cons 'real? real?)
        (cons 'rational? rational?)
        (cons 'numerator numerator)
        (cons 'denominator denominator)
        (cons 'integer? integer?)
        ; system
        (cons 'exit exit)
        (cons 'error error)
        ; io; for more info see https://www.scheme.com/tspl3/io.html
        (cons 'read read)
        (cons 'write write)
        (cons 'port? port?)
        (cons 'eof-object? eof-object?)
        (cons 'display display)
        (cons 'newline newline)
        (cons 'input-port? input-port?)
        (cons 'open-input-file open-input-file)
        (cons 'close-input-port close-input-port)
        (cons 'output-port? output-port?)
        (cons 'open-output-file open-output-file)
        (cons 'close-output-port close-output-port)
        (cons 'read-char read-char)
        (cons 'write-char write-char)
        (cons 'peek-char peek-char)
        (cons 'char-ready? char-ready?)
        ; strings
        (cons 'string? string?)
        (cons 'string-length string-length)
        (cons 'string-ref string-ref)
        (cons 'string-set! string-set!)
        (cons 'string=? string=?)
        (cons 'string-ci=? string-ci=?)
        (cons 'string<? string<?)
        (cons 'string-ci<? string-ci<?)
        (cons 'string<=? string<=?)
        (cons 'string-ci<=? string-ci<=?)
        (cons 'string>? string>?)
        (cons 'string-ci>? string-ci>?)
        (cons 'string>=? string>=?)
        (cons 'string-ci>=? string-ci>=?)
        (cons 'substring substring)
        (cons 'string-append string-append)
        (cons 'string-copy string-copy)
        (cons 'string->number string->number)
        (cons 'number->string number->string)
        (cons 'symbol->string symbol->string)
        (cons 'string->symbol string->symbol)
        ; characters
        (cons 'char? char?)
        (cons 'char-upcase char-upcase)
        (cons 'char-downcase char-downcase)
        (cons 'char-alphabetic? char-alphabetic?)
        (cons 'char-numeric? char-numeric?)
        (cons 'char-alphanumeric? (lambda (c) (or (char-alphabetic? c)
                                                  (char-numeric? c))))
        (cons 'char-whitespace? char-whitespace?)
        (cons 'char-upper-case? char-upper-case?)
        (cons 'char-lower-case? char-lower-case?)
        (cons 'char=? char=?)
        (cons 'char-ci=? char-ci=?)
        (cons 'char<? char<?)
        (cons 'char-ci<? char-ci<?)
        (cons 'char<=? char<=?)
        (cons 'char-ci<=? char-ci<=?)
        (cons 'char>? char>?)
        (cons 'char-ci>? char-ci>?)
        (cons 'char>=? char>=?)
        (cons 'char-ci>=? char-ci>=?)
        (cons 'integer->char integer->char)
        (cons 'char->integer char->integer)
        ; lists
        (cons 'cons cons)
        (cons 'car car)
        (cons 'cdr cdr)
        (cons 'set-car! set-car!)
        (cons 'set-cdr! set-cdr!)
        (cons 'list list)
        (cons 'list? list?)
        (cons 'null? null?)
        (cons 'length length)
        (cons 'append append)
        (cons 'reverse reverse)
        (cons 'list-tail list-tail)
        (cons 'list-ref list-ref)
        (cons 'assq assq)
        (cons 'assv assv)
        (cons 'assoc assoc)
        (cons 'memq memq)
        (cons 'memv memv)
        (cons 'member member)
        (cons 'string->list string->list)
        (cons 'list->string list->string)
        ; math
        (cons 'floor floor)
        (cons 'ceiling ceiling)
        (cons 'truncate truncate)
        (cons 'round round)
        (cons 'square (lambda (x) (* x x)))
        (cons 'abs abs)
        (cons 'gcd gcd)
        (cons 'lcm lcm)
        (cons 'expt expt)
        (cons 'sqrt sqrt)
        (cons 'odd? odd?)
        (cons 'even? even?)
        (cons 'max max)
        (cons 'min min)
        (cons 'exp exp)
        (cons 'log log)
        (cons 'sin sin)
        (cons 'cos cos)
        (cons 'tan tan)
        (cons 'asin asin)
        (cons 'acos acos)
        (cons 'atan atan)
        ; extra
        ; 2
        (cons 'caar caar)
        (cons 'cadr cadr)
        (cons 'cdar cdar)
        (cons 'cddr cddr)
        ; 3
        (cons 'caaar caaar)
        (cons 'caadr caadr)
        (cons 'cadar cadar)
        (cons 'cdaar cdaar)
        (cons 'caddr caddr)
        (cons 'cdadr cdadr)
        (cons 'cddar cddar)
        (cons 'cdddr cdddr)
        ; 4
        (cons 'caaaar caaaar)
        (cons 'caaadr caaadr)
        (cons 'caadar caadar)
        (cons 'cadaar cadaar)
        (cons 'cdaaar cdaaar)
        (cons 'caaddr caaddr)
        (cons 'cadadr cadadr)
        (cons 'caddar caddar)
        (cons 'cdaadr cdaadr)
        (cons 'cdadar cdadar)
        (cons 'cddaar cddaar)
        (cons 'cadddr cadddr)
        (cons 'cdaddr cdaddr)
        (cons 'cddadr cddadr)
        (cons 'cdddar cdddar)
        (cons 'cddddr cddddr)
        ; <...>
))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (prim-proc) (list 'primitive (cdr prim-proc)))
       primitive-procedures))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define (apply-primitive-procedure proc args)
  (debug-log "APPLY-PRIMITIVE" args "->" proc)
  (let ((result (apply-in-underlying-scheme (primitive-implementation proc)
                                            args)))
    (cond ((eq? result '#f) 'false)
          ((eq? result '#t) 'true)
          (else result))))


;; ************************ Read-Eval-Print-Loop *******************************

(define global-environment (setup-environment))

(set-log-verbosity! 'DEBUG) ;; QUIET < ERROR < WARNING < INFO < @DEBUG


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
          (info-log "*** bye! ***")
          (exit))
        (let ((output (evaln input global-environment)))
          (user-print output))))
  (repl))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     "->"
                     (procedure-body object)))
      (display object)))


(info-log "ABRACADABRA [version L-0.1]")

(repl)
