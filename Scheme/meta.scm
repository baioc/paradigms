#!/usr/bin/scheme --script
  ; !# ;; GUILE's mandatory script comment end
  ;; coding: utf-8
  ; (module ufscheme) ;; module declaration for BIGLOO
  ;;

;; ****************************** SOURCE INFO **********************************

  ;; Copyright (c) 2019 Gabriel B. Sant'Anna <baiocchi.gabriel@gmail.com>
  ;;
  ;; @License MIT <https://gitlab.com/baioc/paradigms>
  ;;
  ;; Permission is hereby granted, free of charge, to any person obtaining a
  ;; copy of this software and associated documentation files (the "Software"),
  ;; to deal in the Software without restriction, including without limitation
  ;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
  ;; and/or sell copies of the Software, and to permit persons to whom the
  ;; Software is furnished to do so, subject to the following conditions:
  ;;
  ;; The above copyright notice and this permission notice shall be included in
  ;; all copies or substantial portions of the Software.
  ;;
  ;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  ;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  ;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  ;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  ;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  ;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  ;; DEALINGS IN THE SOFTWARE.

  ;; @Tested with:
  ;; - chez-9.5.2 (Manjaro Linux, x86_64)
  ;; - guile-2.2.6 (Manjaro Linux, x86_64)
  ;; - bigloo-4.3e (Manjaro Linux, x86_64)

  ;; @TODO list:
  ;; - fix analyze-sequence's sequential definitions
  ;;   - see SICP 4.1.6 - Internal Definitions
  ;; - letrec (see SICP Exercise 4.20)
  ;; - more primitives
  ;;   - procedure?, apply, map, for-each, filter
  ;;   - force, delay, eval, apply, load


;; ******************************** LOGGER *************************************

  (define (logger level msgs method)
    (if (<= level *log-level*)
        (begin
          (for-each (lambda (obj) (method obj) (display " ")) msgs)
          (newline)))
    (voidn))

  (define *log-level* 2)


  (define (debug-log . msgs)
    (logger 4 (cons 'DEBUG: msgs) write))

  (define (info-log . msgs)
    (logger 3 (cons 'INFO: msgs) (lambda (msg) (write (prettify msg)))))

  (define (warn-log . msgs)
    (logger 2 (cons ";;; WARNING:" msgs) display))

  (define (error-log . msgs)
    (logger 1 (cons ";;; ERROR:" msgs) display))

  (define (set-log-verbosity! level)
    (cond ((eq? level 'DEBUG) (set! *log-level* 4))
          ((eq? level 'INFO) (set! *log-level* 3))
          ((eq? level 'WARN) (set! *log-level* 2))
          ((eq? level 'ERROR) (set! *log-level* 1))
          ((eq? level 'QUIET) (set! *log-level* 0)))
    (info-log 'Logger 'verbosity 'at 'level level))


;; ************************* GENERIC INTERPRETER *******************************

  (define (ambevaln expr env success failure)
    ((analyze expr) env success failure))

  ;; data-directed style dispatch
  (define (analyze expr)
    (debug-log 'Analyzing expr)
    (cond ;; primitives
          ((self-evaluating? expr) (analyze-self-evaluating expr))
          ((variable? expr) (analyze-variable expr))
          ;; special forms
          ((reserved? expr) (analyze-reserved expr))
          ((ambiguous? expr) (analyze-ambiguous expr))
          ;; combinations
          ((application? expr) (analyze-application expr))
          ;; unknown
          (else (error-log "Unknown expression type -- ANALYZE" expr))))

  (define (execute procedure arguments succeed fail)
    (cond ;; goto primitive routine
          ((primitive-procedure? procedure)
             (succeed (apply-primitive-procedure procedure arguments) fail))
          ;; or execute compund procedure
          ((compound-procedure? procedure)
             (apply-compound-procedure procedure arguments succeed fail))
          ;; fail
          (else (error-log "Unknown procedure type -- EXECUTE" procedure))))


;; *********************** SYNTACTIC ANALYSIS RULES ****************************

  (define (analyze-self-evaluating thing)
    (info-log 'Atom thing)
    (lambda (env succeed fail)
      (succeed thing fail)))

  (define (analyze-variable var)
    (info-log 'Variable var)
    (lambda (env succeed fail)
      (debug-log 'Looking 'up var)
      (succeed (lookup-variable var env) fail)))

  (define (analyze-reserved expr)
    (let ((keyword (reserved-word expr)))
      (debug-log 'Fetching 'syntax 'rules keyword)
      ((reserved-rule keyword) expr)))


  (define (analyze-ambiguous expr)
    (debug-log 'Nondeterministic expr)
    (let ((cprocs (map analyze (amb-choices expr))))
      (lambda (env succeed fail)
        (define (try-next choice-procs choice-list)
          (info-log 'Ambiguously choice-list)
          (if (null? choice-procs)
              (fail)
              ((car choice-procs) env
                                  succeed
                                  (lambda ()
                                    (try-next (cdr choice-procs)
                                              (cdr choice-list))))))
        (try-next cprocs (amb-choices expr)))))


  (define (analyze-application expr)
    (define (eval-args procargs env succeed fail-args)
      (if (null? procargs)
          (succeed procargs fail-args)
          ;; @NOTE: imposes the order of evaluation for procedure arguments (RL)
          ((car procargs) env
                          (lambda (first fail-rest)
                            (eval-args (cdr procargs)
                                       env
                                       (lambda (rest fail)
                                         (succeed (cons first rest) fail))
                                       fail-rest))
                          fail-args)))
    (info-log 'Application expr)
    (let ((funcproc (analyze (operator expr)))
          (argprocs (map analyze (operands expr))))
      (lambda (env succeed fail-func)
        (funcproc env
                  (lambda (func fail-args)
                    (eval-args argprocs
                               env
                               (lambda (args fail-execute)
                                 (execute func args succeed fail-execute))
                               fail-args))
                  fail-func))))


  (define reserved-word-rules
    (list
      ;; simple
      (cons 'lambda (lambda (expr) (analyze-lambda expr)))
      (cons 'define (lambda (expr) (analyze-definition expr)))
      (cons 'set! (lambda (expr) (analyze-assignment expr)))
      (cons 'quote (lambda (expr) (analyze-quotation expr)))
      (cons 'if (lambda (expr) (analyze-if expr)))
      (cons 'begin (lambda (expr) (analyze-sequence (begin-actions expr))))
      ;; derived
      (cons 'cond (lambda (expr) (analyze (cond->if (condition-clauses expr)))))
      (cons 'and (lambda (expr) (analyze (and->if (condition-clauses expr)))))
      (cons 'or (lambda (expr) (analyze (or->if (condition-clauses expr)))))
      (cons 'let (lambda (expr) (analyze (let->combination expr))))
      (cons 'let* (lambda (expr) (analyze (let*->let expr))))
  ))

  (define (reserved-rule keyword)
    (cdr (assq keyword reserved-word-rules)))


  (define (analyze-lambda expr)
    (let ((params (lambda-parameters expr))
          (bodyproc (analyze-sequence (lambda-body expr))))
      (if (not (list? params))
          (error-log "Improper formal argument list" params)
          (lambda (env succeed fail)
            (succeed (make-procedure params bodyproc env) fail)))))

  ;; expression sequentialization is done by nesting
  (define (analyze-sequence exprs)
    (define (sequentially aproc bproc)
      (lambda (env succeed fail-first)
        (aproc env
               (lambda (a-value fail-second)
                 (bproc env succeed fail-second))
               fail-first)))
    (define (unroll first-proc rest-procs)
      (if (null? rest-procs) first-proc
          (unroll (sequentially first-proc (car rest-procs))
                  (cdr rest-procs))))
    (let ((procs (map analyze exprs)))
      (if (null? procs)
          (error-log "Empty symbolic expression" exprs)
          (unroll (car procs) (cdr procs)))))


  (define (analyze-definition expr)
    (let ((var (definition-variable expr))
          (vproc (analyze (definition-value expr))))
      (lambda (env succeed fail-definiens)
        (vproc env
               (lambda (val fail-definition)
                 (succeed (define-variable! var val env)
                          fail-definition))
               fail-definiens))))

  (define (analyze-assignment expr)
    (let ((var (assignment-variable expr))
          (vproc (analyze (assignment-value expr))))
      (lambda (env succeed fail-value)
        (vproc env
               (lambda (val fail-continuation)
                 (let ((old-val (lookup-variable var env)))
                   (if (eq? old-val (voidn))
                       (succeed old-val fail-continuation)
                       (succeed (set-variable-value! var val env)
                                (lambda () ;; undo assignment on failure
                                   (set-variable-value! var old-val env)
                                   (fail-continuation))))))
               fail-value))))


  (define (analyze-quotation expr)
    (let ((qval (quotation expr)))
      (lambda (env succeed fail)
        (succeed qval fail))))


  (define (analyze-if expr)
    (let ((pproc (analyze (if-predicate expr)))
          (cproc (analyze (if-consequent expr)))
          (aproc (analyze (if-alternative expr))))
      (lambda (env succeed fail-pred)
        (pproc env
               (lambda (pred-value fail-branch)
                 ;; truth value of expression may differ in separate languages
                 (if (true? pred-value)
                     (cproc env succeed fail-branch)
                     (aproc env succeed fail-branch)))
               fail-pred))))


  ;; clauses are evaluated right to left, when the predicate is true the actions
  ;; are performed in sequence and their final value is returned;
  ;; an else clause is always executed when found;
  ;; the additional syntax (<test> => <sink>) is such that if <test> evaluates
  ;; to a true value, the one-argument procedure <sink> is applied on it
  (define (cond->if clauses)
    (if (empty-clauses? clauses) 'false ;; no else clause
        (let ((curr (current-clause clauses))
              (rest (rest-clauses clauses)))
          (cond ((cond-else-clause? curr)
                    (if (not (at-last-clause? clauses))
                        (warn-log "ELSE clause isn't last"))
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
    (define (let-reduce associations body)
      (if (null? associations)
          (if (null? (cdr body))
              (car body) body) ;; unwraps lists with a single element
          (make-let (list (car associations))
                    (let-reduce (cdr associations) body))))
    (let-reduce (let-associations expr) (let-body expr)))


;; *********************** SYMBOLIC REPRESENTATION *****************************

  (define (self-evaluating? sexpr)
    (or (number? sexpr)
        (string? sexpr)
        (char? sexpr)
        (vector? sexpr)))

  (define (variable? sexpr)
    (or (symbol? sexpr)
        (boolean? sexpr)))

  (define (reserved? sexpr)
    (and (application? sexpr) ;; "tagged application"
         (assq (reserved-word sexpr) reserved-word-rules)))

  (define (ambiguous? sexpr)
    (tagged-list? sexpr 'amb))

  (define (application? sexpr)
    (pair? sexpr)) ;; "untagged application"

  (define (tagged-list? sexpr tag)
    (and (pair? sexpr)
         (eq? (car sexpr) tag)))


  (define (reserved-word sexpr)
    (car sexpr))


  (define (amb-choices sexpr)
    (cdr sexpr))


  (define (operator sexpr)
    (car sexpr))

  (define (operands sexpr)
    (cdr sexpr))


  (define (make-lambda parameters body)
    (let ((procedure (cons 'lambda (cons parameters body))))
      (info-log 'Making procedure) procedure))

  (define (lambda-parameters sexpr)
    (cadr sexpr))

  (define (lambda-body sexpr)
    (cddr sexpr))


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

  (define (if-alternative sexpr)
    (if (null? (cdddr sexpr)) 'false ;; unspecified
        (cadddr sexpr)))

  ;; @NOTE: anything not equal to the symbol false is "true"
  (define (true? expr)
    (not (eq? expr 'false)))


  (define (make-begin exprs)
    (cons 'begin exprs))

  (define (begin-actions sexpr)
    (cdr sexpr))


  ;; these are shared by COND, AND & OR derivation
  (define (condition-clauses cexpr)
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
    (list 'let associations body))

  (define (let-associations sexpr)
    (cadr sexpr))

  (define (let-body sexpr)
    (cddr sexpr))

  (define (let-vars expr)
    (map car (let-associations expr)))

  (define (let-inits expr)
    (map cadr (let-associations expr)))


;; ***************************** ENVIRONMENTS **********************************

  (define (apply-compound-procedure procedure arguments succeed fail)
    (let ((proc (procedure-body procedure))
          (params (procedure-parameters procedure))
          (base-env (procedure-environment procedure)))
      (let ((num-params (length params))
            (num-args (length arguments)))
        (cond ((< num-args num-params)
                 (warn-log "Too few arguments supplied" params arguments))
              ((> num-args num-params)
                 (warn-log "Too many arguments supplied" params arguments))
              (else
                 (info-log 'Executing 'compound proc 'with params)
                 (proc (extend-environment base-env params arguments)
                       succeed
                       fail))))))

  (define (compound-procedure? sexpr)
    (tagged-list? sexpr 'procedure))

  ;; lambdas/procedures reference the frame where they were defined
  (define (make-procedure parameters body env)
    (list 'procedure parameters body env))

  (define (procedure-parameters sexpr)
    (cadr sexpr))

  (define (procedure-body sexpr)
    (caddr sexpr))

  (define (procedure-environment sexpr)
    (cadddr sexpr))


  (define (lookup-variable var env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars) (env-loop (enclosing-environment env)))
              ((eq? var (car vars)) (car vals))
              (else (scan (cdr vars) (cdr vals)))))
      (if (equal? env the-empty-environment)
          (warn-log "Undefined variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))

  (define (set-variable-value! var val env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars) (env-loop (enclosing-environment env)))
              ((eq? var (car vars))
                (info-log 'Setting var 'to val)
                (set-car! vals val) (voidn))
              (else (scan (cdr vars) (cdr vals)))))
      (if (equal? env the-empty-environment)
          (warn-log "Unbound variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))

  (define (define-variable! var val env)
    (let ((frame (first-frame env)))
      (define (scan vars vals)
        (cond ((null? vars) ;; when var is undefined, locally bind it to val
                 (add-binding-to-frame! var val frame))
              ((eq? var (car vars))
                 (set-car! vals val) (voidn))
              (else (scan (cdr vars) (cdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame))))


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
    (cons variables values))

  (define (frame-variables frame)
    (car frame))

  (define (frame-values frame)
    (cdr frame))

  (define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (frame-variables frame)))
    (set-cdr! frame (cons val (frame-values frame))) (voidn))


  (define (apply-primitive-procedure procedure arguments)
    (let ((proc (primitive-implementation procedure)))
      (info-log 'Applying 'primitive proc 'to arguments)
      (let ((result (apply proc arguments)))
        ;; necessary conversion of primitive representation of truth
        (cond ((eq? result '#f) 'false)
              ((eq? result '#t) 'true)
              (else result)))))

  (define (primitive-procedure? sexpr)
    (tagged-list? sexpr 'primitive))

  (define (primitive-procedure-names)
    (map car primitive-procedures))

  (define (primitive-procedure-objects)
    (map (lambda (prim-proc) (list 'primitive (cdr prim-proc)))
         primitive-procedures))

  (define (primitive-implementation proc)
    (cadr proc))


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

  (define primitive-procedures
    (list ;; minimal
          (cons '+ +)
          (cons '- -)
          (cons '= =)
          (cons '< <)
          (cons '> >)
          (cons 'not ;; logical not needs boolean conversion as well
            (lambda (bool) (if (true? bool) 'false 'true)))
          ;; reduced
          (cons '<= <=)
          (cons '>= >=)
          (cons '* *)
          (cons '/ /)
          (cons 'quotient quotient)
          (cons 'remainder remainder)
          (cons 'modulo modulo)
          (cons 'zero? zero?)
          (cons 'negative? negative?)
          (cons 'positive? positive?)
          (cons '1+
            (lambda (x) (+ x 1)))
          (cons '1-
            (lambda (x) (- x 1)))
          ;; symbolic
          (cons 'equal? equal?)
          (cons 'eqv? eqv?)
          (cons 'eq? eq?)
          (cons 'symbol? symbol?)
          (cons 'pair? pair?)
          (cons 'boolean?
            (lambda (expr) (or (eq? expr 'true) (eq? expr 'false))))
          ;; numeric
          (cons 'number? number?)
          (cons 'inexact? inexact?)
          (cons 'exact? exact?)
          (cons 'inexact->exact inexact->exact)
          (cons 'exact->inexact exact->inexact)
          (cons 'complex? complex?)
          (cons 'real? real?)
          (cons 'rational? rational?)
          (cons 'integer? integer?)
          (cons 'floor floor)
          (cons 'ceiling ceiling)
          (cons 'truncate truncate)
          (cons 'round round)
          (cons 'nan?
            (lambda (x) (not (= x x))))
          ;; lists
          (cons 'cons cons)
          (cons 'car car)
          (cons 'cdr cdr)
          (cons 'set-car!
            (lambda (pair val) (set-car! pair val) (voidn)))
          (cons 'set-cdr!
            (lambda (pair val) (set-cdr! pair val) (voidn)))
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
          ;; math
          (cons 'square
            (lambda (x) (* x x)))
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
          ;; system
          (cons 'exit
            (lambda (code) (bye code)))
          (cons 'meta-set-log-verbosity! set-log-verbosity!)
          ;; io, for more info see https://www.scheme.com/tspl3/io.html
          (cons 'read read)
          (cons 'write
            (lambda (obj . port) (if (null? port)
                                     (write obj)
                                     (write obj (car port)))
                                 (voidn)))
          (cons 'port? port?)
          (cons 'eof-object? eof-object?)
          (cons 'display
            (lambda (obj . port) (if (null? port)
                                     (display obj)
                                     (display obj (car port)))
                                 (voidn)))
          (cons 'newline
            (lambda port (if (null? port)
                             (newline)
                             (newline (car port)))
                         (voidn)))
          (cons 'input-port? input-port?)
          (cons 'open-input-file open-input-file)
          (cons 'close-input-port
            (lambda (port) (close-input-port port) (voidn)))
          (cons 'call-with-input-file call-with-input-file)
          (cons 'current-input-port current-input-port)
          (cons 'with-input-from-file with-input-from-file)
          (cons 'output-port? output-port?)
          (cons 'open-output-file open-output-file)
          (cons 'close-output-port
            (lambda (port) (close-output-port port) (voidn)))
          (cons 'call-with-output-file call-with-output-file)
          (cons 'current-output-port current-output-port)
          (cons 'with-output-to-file with-output-to-file)
          (cons 'read-char read-char)
          (cons 'write-char
            (lambda (char . port) (if (null? port)
                                      (write-char char)
                                      (write-char char (car port)))
                                  (voidn)))
          (cons 'peek-char peek-char)
          (cons 'char-ready? char-ready?)
          ;; strings
          (cons 'string? string?)
          (cons 'make-string make-string)
          (cons 'string-length string-length)
          (cons 'string-ref string-ref)
          (cons 'string-set!
            (lambda (str pos char) (string-set! str pos char) (voidn)))
          (cons 'string-fill!
            (lambda (str char) (string-fill! str char) (voidn)))
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
          ;; characters
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
          (cons 'char-alphanumeric?
            (lambda (c) (or (char-alphabetic? c) (char-numeric? c))))
          ;; vectors
          (cons 'vector? vector?)
          (cons 'vector vector)
          (cons 'make-vector make-vector)
          (cons 'vector-length vector-length)
          (cons 'vector-ref vector-ref)
          (cons 'vector-set!
            (lambda (vec pos val) (vector-set! vec pos val) (voidn)))
          (cons 'vector-fill!
            (lambda (vec val) (vector-fill! vec val) (voidn)))
          (cons 'vector->list vector->list)
          (cons 'list->vector list->vector)
          ;; extra CAR-CDRing
          ;; 2
          (cons 'caar caar)
          (cons 'cadr cadr)
          (cons 'cdar cdar)
          (cons 'cddr cddr)
          ;; 3
          (cons 'caaar caaar)
          (cons 'caadr caadr)
          (cons 'cadar cadar)
          (cons 'cdaar cdaar)
          (cons 'caddr caddr)
          (cons 'cdadr cdadr)
          (cons 'cddar cddar)
          (cons 'cdddr cdddr)
          ;; 4
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


;; ************************ COMMAND LINE INTERFACE *****************************

  (define (read-eval-print-loop) ;; REPL
    (define (loop retry)
      (display ">> ")
      (let ((input (read)))
        (cond ((eof-object? input) (bye 0))
              ((eq? input 'retry) (retry))
              (else (ambevaln (simplify input)
                              global-environment
                              (lambda (output continue)
                                (repl-print output)
                                (loop continue))
                              (lambda ()
                                (display ";;; No more possible outcomes for ")
                                (repl-print input)
                                (read-eval-print-loop)))))))
    (loop (lambda ()
            (display ";;; There are no currently unresolved ambiguities\n")
            (read-eval-print-loop))))

  (define (repl-print obj)
    (cond ((compound-procedure? obj)
             (write (procedure-body obj))
             (newline))
          ((primitive-procedure? obj)
             (write (primitive-implementation obj))
             (newline))
          ((eq? obj (voidn))
             (display ""))
          (else (write (prettify obj))
                (newline))))


  (define (voidn)
    '*void*)

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


  (define (bye return-code)
    (newline)
    (info-log 'Shut-down...)
    (display ";;; bye!\n")
    (exit return-code))

  ; (set-log-verbosity! 'INFO) ;; QUIET < ERROR < [WARN] < INFO < @DEBUG

  (define global-environment (setup-environment))

  (info-log 'Greetings!)
  (display " _   _______ _____ _____  _   _  ________  ___ _____\n")
  (display "| | | |  ___/  ___/  __ \\| | | ||  ___|  \\/  ||  ___|\n")
  (display "| | | | |_  \\ `--.| /  \\/| |_| || |__ | .  . || |__\n")
  (display "| | | |  _|  `--. \\ |    |  _  ||  __|| |\\/| ||  __|\n")
  (display "| |_| | |   /\\__/ / \\__/\\| | | || |___| |  | || |___\n")
  (display " \\___/\\_|   \\____/ \\____/\\_| |_/\\____/\\_|  |_/\\____/\n")
  (display ";;; UFSCheme Version 0.3.bb\n")
  (display ";;; (c) 2019, Gabriel B. Sant'Anna <baiocchi.gabriel@gmail.com>\n")

  (read-eval-print-loop)

  (bye 0)
