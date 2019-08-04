;; ****************************** SOURCE INFO **********************************
;; Copyright (c) 2019 Gabriel B. Sant'Anna
;;
;; @License MIT <https://gitlab.com/baioc/paradigms>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; @Tested with:
;; chez-9.5.2
;; petite-9.5.2
;; guile-2.2.6
;; bigloo-4.3e


;; ******************************** LOGGER *************************************
(define (logger level msgs method)
  (if (<= level *log-level*)
      (begin
        (for-each (lambda (obj) (method obj) (display " ")) msgs)
        (newline)))
  '*void*)

(define *log-level* 2)


(define (debug-log . msgs)
  (logger 4 (cons 'DEBUG: msgs) write))

(define (info-log . msgs)
  (logger 3 (cons 'INFO: msgs) (lambda (msg) (write (prettify msg)))))

(define (warn-log . msgs)
  (logger 2 (cons 'WARNING: msgs) display))

(define (error-log . msgs)
  (logger 1 (cons 'ERROR: msgs) display))

(define (set-log-verbosity! level)
  (cond ((eq? level 'DEBUG) (set! *log-level* 4))
        ((eq? level 'INFO) (set! *log-level* 3))
        ((eq? level 'WARN) (set! *log-level* 2))
        ((eq? level 'ERROR) (set! *log-level* 1))
        ((eq? level 'QUIET) (set! *log-level* 0)))
  (info-log 'Logger 'verbosity 'at 'level level))


;; ************************* GENERIC INTERPRETER *******************************

(define (evaln expr env)
  ((analyze expr) env))

;; data-directed style dispatch
(define (analyze expr)
  (debug-log 'Analyzing expr)
  (cond ;; primitives
        ((self-evaluating? expr) (analyze-self-evaluating expr))
        ((variable? expr) (analyze-variable expr))
        ;; combinations
        ((reserved? expr) (analyze-reserved expr))
        ((application? expr) (analyze-application expr))
        ;; unknown
        (else (error-log "Unknown expression type -- ANALYZE" expr))))

(define (execute procedure arguments)
  (cond ;; goto primitive routine
        ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
        ;; or execute compund procedure
        ((compound-procedure? procedure)
           (apply-compound-procedure procedure arguments))
        ;; fail
        (else (error-log "Unknown procedure type -- EXECUTE" procedure))))


;; *********************** SYNTACTIC ANALYSIS RULES ****************************

(define (analyze-self-evaluating thing)
  (info-log 'Atom thing)
  (lambda (env) thing))

(define (analyze-variable var)
  (lambda (env)
    (info-log 'Looking 'up var)
    (lookup-variable var env)))

(define (analyze-reserved expr)
  (let ((keyword (reserved-word expr)))
    (debug-log 'Fetching 'syntax 'rules keyword)
    ((reserved-rule keyword) expr)))

(define (analyze-application expr)
  (define (eval-args procargs env)
    (if (null? procargs) procargs
        ;; @NOTE: this sets the order of evaluation for procedure arguments
        (let* (
               (rest (eval-args (cdr procargs) env))
               (first ((car procargs) env))
              )
          (cons first rest))))
  (let ((funcproc (analyze (operator expr)))
        (argprocs (map analyze (operands expr))))
    (lambda (env) (execute (funcproc env) (eval-args argprocs env)))))


(define reserved-word-rules
  (list
    (cons 'lambda (lambda (expr) (analyze-lambda expr)))
    (cons 'define (lambda (expr) (analyze-definition expr)))
    (cons 'set! (lambda (expr) (analyze-assignment expr)))
    (cons 'quote (lambda (expr) (analyze-quotation expr)))
    (cons 'if (lambda (expr) (analyze-if expr)))
    (cons 'begin (lambda (expr) (analyze-sequence (begin-actions expr))))
    (cons 'cond (lambda (expr) (analyze (cond->if (conditional-clauses expr)))))
    (cons 'and (lambda (expr) (analyze (and->if (conditional-clauses expr)))))
    (cons 'or (lambda (expr) (analyze (or->if (conditional-clauses expr)))))
    (cons 'let (lambda (expr) (analyze (let->combination expr))))
    (cons 'let* (lambda (expr) (analyze (let*->let expr))))
))

(define (reserved-rule keyword)
  (cdr (assq keyword reserved-word-rules)))


(define (analyze-lambda expr)
  (let ((params (lambda-parameters expr))
        (body (analyze-sequence (lambda-body expr))))
    (if (not (list? params))
        (warn-log "Improper formal argument list" params)
        (lambda (env) (make-procedure params body env)))))

;; expression sequentialization is done by nesting
(define (analyze-sequence exprs)
  (define (unroll first-proc rest-procs)
    (if (null? rest-procs) first-proc
        (unroll (lambda (env) (first-proc env)
                              ((car rest-procs) env))
                (cdr rest-procs))))
  (let ((procs (map analyze exprs)))
    (if (null? procs)
        (warn-log "Empty symbolic expression" exprs)
        (unroll (car procs) (cdr procs)))))


(define (analyze-definition expr)
  (let ((var (definition-variable expr))
        (vproc (analyze (definition-value expr))))
    (lambda (env)
      (define-variable! var (vproc env) env))))

(define (analyze-assignment expr)
  (let ((var (assignment-variable expr))
        (vproc (analyze (assignment-value expr))))
    (lambda (env)
      (set-variable-value! var (vproc env) env))))


(define (analyze-quotation expr)
  (let ((qval (quotation expr)))
    (lambda (env) qval)))


(define (analyze-if expr)
  (let ((pproc (analyze (if-predicate expr)))
        (cproc (analyze (if-consequent expr)))
        (aproc (analyze (if-alternative expr))))
    (lambda (env)
      ;; truth value of expression may differ in separate languages
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))


;; clauses are evaluated right to left, when the predicate is true the actions
;; are performed in sequence and their final value is returned; an else clause
;; is always executed when found.
;; the pipe additional syntax (<test> => <sink>) works such that if <test> gets
;; evaluated to a true value, the one-argument procedure <sink> is applied on it
(define (cond->if clauses)
  (if (empty-clauses? clauses) 'false ;; no else clause
      (let ((curr (current-clause clauses))
            (rest (rest-clauses clauses)))
        (cond ((cond-else-clause? curr)
                  (if (not (at-last-clause? clauses))
                      (warn-log "ELSE clause isn't last" (cons 'cond clauses)))
                  (make-begin (cond-actions curr)))
              ((cond-pipe-clause? curr)
                  (make-let (list (list '*temp* (cond-predicate curr)))
                            (make-if '*temp*
                                     (list (cond-pipe-action curr) '*temp*)
                                     (cond->if rest))))
              (else (make-if (cond-predicate curr)
                             (make-begin (cond-actions curr))
                             (cond->if rest)))))))

;; when a clause evaluates to false, returns false immediately;
;; when every clause evaluates to true, returns the value of the last one;
;; when there are no clauses, returns true
(define (and->if clauses)
  (if (empty-clauses? clauses) 'true
      (make-let (list (list '*temp* (current-clause clauses)))
                (make-if '*temp*
                         (if (at-last-clause? clauses) '*temp*
                             (and->if (rest-clauses clauses)))
                         'false))))

;; when a clause evaluates to true, returns its value immediately;
;; when every clause evaluates to false, returns false;
;; when there are no clauses, returns false
(define (or->if clauses)
  (if (empty-clauses? clauses) 'false
      (make-let (list (list '*temp* (current-clause clauses)))
                (make-if '*temp*
                         '*temp*
                         (or->if (rest-clauses clauses))))))


;; since let defines a local scope, it is derived as a lambda combination
;; whose arguments are immediately bound to the initializing parameters
(define (let->combination expr)
  (cons (make-lambda (let-vars expr)
                     (let-body expr))
        (let-inits expr)))

;; let* imposes order, thus it may be constructed by nesting lets
(define (let*->let expr)
  (define (let-reduce associations exprs)
    (if (null? associations)
        (if (null? (cdr exprs)) (car exprs)
            exprs) ;; unwraps lists with a single element
        (make-let (list (car associations))
                  (let-reduce (cdr associations) exprs))))
  (let-reduce (let-associations expr) (let-body expr)))


;; *********************** SYMBOLIC REPRESENTATION *****************************

(define (self-evaluating? sexpr)
  (or (number? sexpr)
      (string? sexpr)
      (char? sexpr)))

(define (variable? sexpr)
  (or (symbol? sexpr)
      (boolean? sexpr)))

(define (reserved? sexpr) ;; tagged
  (and (application? sexpr)
       (assq (reserved-word sexpr) reserved-word-rules)))

(define (application? sexpr) ;; untagged
  (pair? sexpr))


(define (compound-procedure? sexpr)
  (tagged-list? sexpr 'procedure))

(define (primitive-procedure? sexpr)
  (tagged-list? sexpr 'primitive))

(define (tagged-list? sexpr tag)
  (and (pair? sexpr)
       (eq? (car sexpr) tag)))


(define (reserved-word sexpr)
  (car sexpr))


(define (operator sexpr)
  (car sexpr))

(define (operands sexpr)
  (cdr sexpr))


(define (make-lambda parameters body)
  (debug-log 'Making 'lambda parameters '-> body)
  (cons 'lambda (cons parameters body)))

(define (lambda-parameters sexpr)
  (cadr sexpr))

(define (lambda-body sexpr)
  (cddr sexpr))


;; @DELETE
; (define (definition? sexpr)
;   (tagged-list? sexpr 'define))

(define (definition-variable sexpr)
  (let ((definiendum (cadr sexpr)))
    (if (variable? definiendum) ;; direct variable definition
        definiendum
        (car definiendum)))) ;; procedure definition

(define (definition-value sexpr)
  (let ((definiendum (cadr sexpr))
        (definiens (cddr sexpr)))
    (if (variable? definiendum)
        (car definiens)
        (make-lambda (cdr definiendum) ;; formal arguments
                     definiens))))     ;; body


;; @DELETE
; (define (make-set variable value)
;   (debug-log 'Making 'assignment variable '= value)
;   (list 'set! variable value))

(define (assignment-variable sexpr)
  (cadr sexpr))

(define (assignment-value sexpr)
  (caddr sexpr))


(define (quotation sexpr)
  (cadr sexpr))


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (if-predicate sexpr)
  (cadr sexpr))

(define (if-consequent sexpr)
  (caddr sexpr))

;; @NOTE: the value of an if expression when the predicate is false and there is
;; no alternative is unspecified in Scheme; we have chosen to make it false
(define (if-alternative sexpr)
  (if (null? (cdddr sexpr)) 'false
      (cadddr sexpr)))

;; @NOTE: anything not equal to the symbol false is "true"
(define (true? expr)
  (not (eq? expr 'false)))


(define (make-begin exprs)
  (cons 'begin exprs))

(define (begin-actions sexpr)
  (cdr sexpr))


;; these are shared by COND, AND & OR derivation
(define (conditional-clauses cexpr)
  (cdr cexpr))

(define (current-clause cls)
  (car cls))

(define (rest-clauses cls)
  (cdr cls))

(define (empty-clauses? cls)
  (null? cls))

(define (at-last-clause? cls)
  (empty-clauses? (rest-clauses cls)))


(define (cond-actions clause)
  (cdr clause))

(define (cond-predicate clause)
  (car clause))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-pipe-clause? clause)
  (eq? (car (cond-actions clause)) '=>))

(define (cond-pipe-action clause)
  (cadr (cond-actions clause)))


(define (make-let associations body)
  (debug-log 'Letting associations 'into 'scope 'of body)
  (list 'let associations body))

(define (let-associations sexpr)
  (cadr sexpr))

(define (let-body sexpr)
  (cddr sexpr))

(define (let-vars expr)
  (map car (let-associations expr)))

(define (let-inits expr)
  (map cadr (let-associations expr)))


; (lambda <vars>
;   (define u <e1>)
;   (define v <e2>)
;   <e3>)
;; =>
; (lambda <vars>
; (let ((u '*unassigned*)
;       (v '*unassigned*))
;   (set! u <e1>)
;   (set! v <e2>)
;   <e3>))
; (define (scan-out-defines body)
;   (define (declare-defs defines)
;       (map (lambda (def) (list (definition-variable def)
;                                '(quote *unassigned*)))
;            defines))
;   (define (initialize-defs defines)
;     (map (lambda (def) (make-set (definition-variable def)
;                                  (definition-value def)))
;          defines))
;   (define (defines->let sexprs defs ndefs)
;     (debug-log "DEFINES->LET" "\n  " sexprs "\n  " defs "\n  " ndefs)
;     (cond ((null? sexprs)
;            (if (null? defs) body
;                (let* ((_ (debug-log "**DEFINES->MAKING-LET**"))
;                       (declarations (declare-defs defs))
;                       (initializations (initialize-defs defs))
;                       (rest-of-body (reverse ndefs)))
;                 (debug-log "  " declarations (append initializations rest-of-body))
;                 (let->combination (make-let declarations (append initializations rest-of-body))))))
;           ((definition? (car sexprs))
;            (debug-log "Internalizing" (car sexprs) "into" (cdr sexprs))
;            (defines->let (cdr sexprs)
;                          (cons (car sexprs) defs)
;                          ndefs))
;           (else (defines->let (cdr sexprs)
;                               defs
;                               (cons (car sexprs) ndefs)))))
;   (let ((out (defines->let body '() '())))
;     (debug-log "DEFINES->LET result:" out)
;     out))
;; @DELETE


;; ****************************** ENVIRONMENT **********************************

;; lambdas/procedures reference the frame where they were defined
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (procedure-parameters sexpr)
  (cadr sexpr))

(define (procedure-body sexpr)
  (caddr sexpr))

(define (procedure-environment sexpr)
  (cadddr sexpr))

(define (apply-compound-procedure procedure arguments)
  ((procedure-body procedure)
   (extend-environment (procedure-environment procedure)
                       (procedure-parameters procedure)
                       arguments)))


(define the-empty-environment
  '())

(define (extend-environment env vars vals)
  (cons (make-frame vars vals) env))

(define (first-frame env)
  (car env))

(define (enclosing-environment env)
  (cdr env))


;; a frame binds local variables to their values
(define (make-frame variables values)
  (let ((numvars (length variables))
        (numvals (length values)))
    (cond ((= numvars numvals)
             (cons variables values))
          ((< numvars numvals)
             (warn-log "Too many arguments supplied" variables values))
          ((> numvars numvals)
             (warn-log "Too few arguments supplied" variables values)))))
;; @TODO this check elsewhere?

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))

(define (lookup-variable var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) ;; crawl up to the outermost scope
               (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
               (if (eq? (car vals) '*unassigned*)
                   (error-log "Unassigned variable" var)
                   (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (equal? env the-empty-environment)
        (warn-log "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) ;; set variable not found locally? check outer scope
               (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
               (set-car! vals val) '*void*)
            (else (scan (cdr vars) (cdr vals)))))
    (if (equal? env the-empty-environment)
        (warn-log "Unbound variable!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) ;; when var is undefined, locally bind it to val
               (add-binding-to-frame! var val frame) '*void*)
            ((eq? var (car vars))
               (set-car! vals val) '*void*)
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


(define (setup-environment)
  (let ((initial-env
        (extend-environment the-empty-environment
                            (primitive-procedure-names)
                            (primitive-procedure-objects))))
    (define-variable! 'true 'true initial-env)
    (define-variable! 'false 'false initial-env)
    (info-log 'Setting-up 'environment...
              (frame-variables (first-frame initial-env)))
    initial-env))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (prim-proc) (list 'primitive (cdr prim-proc)))
       primitive-procedures))

(define (primitive-implementation proc)
  (cadr proc))

(define (apply-primitive-procedure proc args)
  (let ((primpl (primitive-implementation proc)))
    (info-log 'Applying 'primitive primpl 'to args)
    (let ((result (apply primpl args)))
      ;; @XXX: necessary conversion of primitive representation of truth
      (cond ((eq? result '#f) 'false)
            ((eq? result '#t) 'true)
            (else result)))))

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
        (cons 'zero? zero?)
        (cons 'negative? negative?)
        (cons 'positive? positive?)
        (cons 'quotient quotient)
        (cons 'remainder remainder)
        (cons 'modulo modulo)
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
        (cons 'real? real?)
        (cons 'rational? rational?)
        (cons 'integer? integer?)
        (cons 'nan? (lambda (x) (not (= x x))))
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
        (cons 'char-alphanumeric? (lambda (c) (or (char-alphabetic? c)
                                                  (char-numeric? c))))
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
        ; extra CAR-CDRing
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
))


;; ************************ Command Line Interface *****************************

(define (repl)
  (define (loop)
    (repl-prompt)
    (let ((input (simplify (read)))) ;; read
      (if (not (eof-object? input))
          (let ((output (evaln input global-environment))) ;; eval
            (repl-print output) ;; print
            (loop))))) ;; loop
  (info-log 'Greetings!)
  (display repl-greet)
  (loop)
  (newline)
  (info-log 'Shut-down...)
  (display repl-bye)
  (exit))

(define (simplify msg)
  (let-replace '((λ lambda)) msg))

(define (prettify msg)
  (let-replace '((lambda λ)) msg))

(define (let-replace swaps sexpr)
  (define (let-replace-unary find replace input)
    (cond ((eq? input find) replace)
          ((list? input) (map (lambda (i) (let-replace-unary find replace i))
                              input))
          (else input)))
  (if (null? swaps) sexpr
      (let-replace (cdr swaps)
                      (let-replace-unary (caar swaps) (cadar swaps) sexpr))))

(define (repl-print obj)
  (cond ((compound-procedure? obj)
           (write (procedure-body obj))
           (newline))
        ((primitive-procedure? obj)
           (write (primitive-implementation obj))
           (newline))
        ((eq? obj '*void*)
           (display ""))
        (else (write (prettify obj))
              (newline))))


(define repl-greet
"  ___ ____________  ___  _____  ___ ______  ___ ____________  ___
 / _ \\| ___ | ___ \\/ _ \\/  __ \\/ _ \\|  _  \\/ _ \\| ___ | ___ \\/ _ \\
/ /_\\ | |_/ | |_/ / /_\\ | /  \\/ /_\\ | | | / /_\\ | |_/ | |_/ / /_\\ \\
|  _  | ___ |    /|  _  | |   |  _  | | | |  _  | ___ |    /|  _  |
| | | | |_/ | |\\ \\| | | | \\__/| | | | |/ /| | | | |_/ | |\\ \\| | | |
\\_| |_\\____/\\_| \\_\\_| |_/\\____\\_| |_|___/ \\_| |_\\____/\\_| \\_\\_| |_/
(c) 2019, Gabriel B. Sant'Anna
Version 0.3.bb\n")

(define (repl-prompt) (display ">> "))

(define repl-bye "*** bye! ***\n")


(set-log-verbosity! 'DEBUG) ;; QUIET < ERROR < [WARN] < INFO < @DEBUG

(define global-environment (setup-environment))

(repl)
