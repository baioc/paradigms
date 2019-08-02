;; ****************************** SOURCE INFO **********************************
;; Copyright (c) 2019 Gabriel B. Sant'Anna
;;
;; @license MIT <https://gitlab.com/baioc/paradigms>
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


;; ******************************** LOGGER *************************************
(define (logger level msgs method)
  (if (<= level *log-level*)
      (begin
        (map (lambda (obj) (method obj) (display " ")) msgs)
        (newline))))

(define *log-level* 2)


(define (debug-log . msgs)
  (logger 4 (cons 'DEBUG: msgs) write))

(define (info-log . msgs)
  (logger 3 (cons 'INFO: msgs) write))

(define (warn-log . msgs)
  (logger 2 (cons 'WARNING: msgs) display))

(define (error-log . msgs)
  (logger 1 (cons 'ERROR: msgs) display))

(define (set-log-verbosity! level)
  (cond ((eq? level 'DEBUG) (set! *log-level* 4))
        ((eq? level 'INFO) (set! *log-level* 3))
        ((eq? level 'WARN) (set! *log-level* 2))
        ((eq? level 'ERROR) (set! *log-level* 1))
        ((eq? level 'QUIET) (set! *log-level* 0))))


;; ************************* GENERIC INTERPRETER *******************************

;; data-directed style dispatch
(define (analyze expr)
  (debug-log 'Analyzing expr)
  (cond ;; primitives
        ((self-evaluating? expr) (analyze-self-evaluating expr))
        ((variable? expr) (analyze-variable expr))
        ;; combinations
        ((reserved? expr) (analyze-syntax-rules expr))
        ((application? expr) (analyze-application expr))
        ;; unknown
        (else (error-log "Unknown expression type -- ANALYZE" expr))))

(define (execute procedure arguments)
  (cond ;; goto primitive routine
        ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ;; or execute compund procedure
        ((compound-procedure? procedure)
         ((procedure-body procedure)
          (extend-environment (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure))))
        ;; fail
        (else (error-log "Unknown procedure type -- EXECUTE" procedure))))


;; *********************** SYNTACTIC ANALYSIS RULES ****************************

(define reserved-word-rules
  (list
    (cons 'lambda (lambda (expr) (analyze-lambda expr)))
    (cons 'define (lambda (expr) (analyze-definition expr)))
    (cons 'set! (lambda (expr) (analyze-assignment expr)))
    (cons 'quote (lambda (expr) (analyze-quotation expr)))
    (cons 'if (lambda (expr) (analyze-if expr)))
    (cons 'begin (lambda (expr) (analyze-sequence (begin-actions expr))))
    (cons 'cond (lambda (expr) (analyze (cond->if expr))))
    (cons 'and (lambda (expr) (analyze-and (and-clauses expr))))
    (cons 'or (lambda (expr) (analyze-or (or-clauses expr))))
    (cons 'let (lambda (expr) (analyze (let->combination expr))))
    (cons 'let* (lambda (expr) (analyze (let*->let expr))))
))

(define (reserved? sexpr)
  (and (pair? sexpr)
       (assq (car sexpr) reserved-word-rules)))

(define (analyze-syntax-rules sexpr)
  (let ((keyword (car sexpr)))
    (debug-log 'Fetching 'syntax 'rules 'for keyword)
    ((cdr (assq keyword reserved-word-rules)) sexpr)))


(define (analyze-self-evaluating sexpr)
  (info-log 'Atom sexpr)
  (lambda (env) sexpr))

(define (analyze-variable var)
  (lambda (env)
    (info-log 'Looking 'up var)
    (lookup-variable var env)))


(define (analyze-application expr)
  (let ((operator (operator expr))
        (operands (operands expr)))
    (info-log 'Analyzing 'application (cons operator operands))
    (let ((funcproc (analyze operator))
          (argprocs (map analyze operands)))
      (lambda (env)
        (info-log 'Executing (cons operator operands))
        (execute (funcproc env)
                 (map (lambda (aproc) (aproc env))
                      argprocs))))))


(define (analyze-lambda expr)
  (let ((params (lambda-parameters expr))
        (bproc (analyze-sequence (lambda-body expr))))
    (lambda (env) (make-procedure params bproc env))))

(define (analyze-sequence sexprs)
  ; unroll(a (b c)) ->
  ; (lambda (env)
  ;   ((lambda (env)
  ;      (a env)
  ;      (b env)) env)
  ;   (c env))
  (define (unroll first-proc rest-procs)
    (if (null? rest-procs) first-proc
        (unroll (lambda (env) (first-proc env) ((car rest-procs) env))
                (cdr rest-procs))))
  (let ((procs (map analyze sexprs)))
    (unroll (car procs) (cdr procs))))


(define (analyze-definition expr)
  (let ((val (definition-value expr)))
    (let ((var (definition-variable expr))
          (vproc (analyze val)))
      (lambda (env)
        (info-log 'Define var ': val)
        (define-variable! var (vproc env) env)))))

(define (analyze-assignment expr)
  (let ((val (assignment-value expr)))
    (let ((var (assignment-variable expr))
          (vproc (analyze val)))
      (lambda (env)
        (info-log 'Assign var '= val)
        (set-variable-value! var (vproc env) env)))))


(define (analyze-quotation expr)
  (let ((qval (quotation expr)))
    (lambda (env) qval)))


;; @NOTE: true? is abstracted away because the truth value of an expression
;; may differ between the interpreter's and the implemented languages
(define (analyze-if expr)
  (let ((predicate (if-predicate expr))
        (consequent (if-consequent expr))
        (alternative (if-alternative expr)))
    (let ((pproc (analyze predicate))
          (cproc (analyze consequent))
          (aproc (analyze alternative)))
      (lambda (env)
        (info-log 'Conditionally predicate '? consequent ': alternative)
        (if (true? (pproc env))
            (cproc env)
            (aproc env))))))

(define (analyze-and clauses)
  (define (eval-and clauses env)
    (if (empty-clauses? clauses) 'true
        (let ((result (evaln (first-clause clauses) env)))
          (cond ((false? result) 'false)
                ((last-clause? clauses) result)
                (else (eval-and (rest-clauses clauses) env))))))
  (lambda (env) (eval-and clauses env)))

;; @FIXME: these two

(define (analyze-or clauses)
  (define (eval-or clauses env)
    (if (empty-clauses? clauses) 'false
        (let ((result (evaln (first-clause clauses) env)))
          (cond ((true? result) result)
                (else (eval-or (rest-clauses clauses) env))))))
  (lambda (env) (eval-or clauses env)))


;; *********************** SYMBOLIC REPRESENTATION *****************************

(define (self-evaluating? sexpr)
  (or (number? sexpr)
      (string? sexpr)
      (char? sexpr)
      (null? sexpr)))

(define (variable? sexpr)
  (or (symbol? sexpr)
      (boolean? sexpr)))

(define (tagged-list? sexpr tag)
  (and (pair? sexpr)
       (eq? (car sexpr) tag)))


(define (application? sexpr) ;; untagged
  (pair? sexpr))

(define (operator sexpr)
  (car sexpr))

(define (operands sexpr)
  (cdr sexpr))


(define (make-lambda parameters body)
  (debug-log 'Making 'lambda parameters '-> body)
  (cons 'lambda (cons parameters body)))

(define (lambda-parameters sexpr)
  (let ((params (cadr sexpr)))
    (if (list? params) params
        (begin (warn-log "Improper formal argument list" params)
               (list params)))))

(define (lambda-body sexpr)
  (cddr sexpr))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? sexpr)
  (tagged-list? sexpr 'procedure))

(define (procedure-parameters proc)
  (cadr proc))

(define (procedure-body proc)
  (caddr proc))

(define (procedure-environment proc)
  (cadddr proc))


;; @DELETE
; (define (definition? sexpr)
;   (tagged-list? sexpr 'define))

(define (definition-variable sexpr)
  (let ((definiendum (cadr sexpr)))
    (if (variable? definiendum)
        definiendum          ;; direct definition
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
  (cons 'if (cons predicate (cons consequent alternative))))

(define (if-predicate sexpr)
  (cadr sexpr))

(define (if-consequent sexpr)
  (caddr sexpr))

;; @NOTE: the value of an if expression when the predicate is false and there is
;; no alternative is unspecified in Scheme; we have chosen here to make it false
(define (if-alternative sexpr)
  (if (null? (cdddr sexpr)) 'false
      (cadddr sexpr)))

(define (true? predicate)
  (not (false? predicate)))

(define (false? predicate)
  ;; @NOTE (quote false) === false
  (eq? predicate 'false))


(define (make-begin seq)
  (list 'begin seq))

(define (begin-actions sexpr)
  (let ((actions (cdr sexpr)))
    (if (null? actions)
        (list actions)
        actions)))

(define (sequence->expr seq)
  (debug-log 'Wrapping seq)
  (cond ((null? seq) seq)
        ((null? (cdr seq)) (car seq))
        (else (make-begin seq))))


(define (cond->if expr)
  (cond-expand-clauses (cond-clauses expr)))

(define (cond-clauses sexpr)
  (cdr sexpr))

(define (cond-predicate clause)
  (car clause))

;; additional syntax for cond clauses: (<test> => <recipient>); if <test>
;; evaluates to a true value, then <recipient> is evaluated: its value must be a
;; procedure of one argument; this procedure is then invoked on <test>. eg:
;; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;       (else false)) -> 2
;; @FIXME: as it is, <test> gets evaluated twice, once during the predicate
;; evaluation and then when applying <recipient> to it
(define (cond-actions clause)
  (let ((actions (cdr clause)))
    (if (eq? (car actions) '=>)
        (list (cadr actions) (cond-predicate clause))
        actions)))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-expand-clauses clauses)
    (if (null? clauses) clauses ;; no else clause
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (cond ((cond-else-clause? first)
                 (if (not (null? rest))
                     (warn-log "ELSE clause isn't last -- COND" clauses)
                     (sequence->expr (cond-actions first))))
                (else (make-if (cond-predicate first)
                               (sequence->expr (cond-actions first))
                               (cond-expand-clauses rest)))))))


(define (and-clauses sexpr)
  (cdr sexpr))

(define (or-clauses sexpr)
  (cdr sexpr))

(define (first-clause lst)
  (car lst))

(define (rest-clauses lst)
  (cdr lst))

(define (empty-clauses? clauses)
  (null? clauses))

(define (last-clause? lst)
  (empty-clauses? (rest-clauses lst)))


(define (make-let associations body)
  (let ((seq (sequence->expr body)))
    (debug-log 'Letting associations 'into 'scope seq)
    (list 'let associations seq)))

(define (let-associations sexpr)
  (cadr sexpr))

(define (let-body sexpr)
  (cddr sexpr))

(define (let-vars expr)
  (map car (let-associations expr)))

(define (let-inits expr)
  (map cadr (let-associations expr)))

;(let ((<var1> <exp1>)
;        ...
;      (<varn> <expn>))
;  <body>)
;; =>
;((lambda (<var1> ... <varn>)
;   <body>)
; <exp1>
;  ...
; <expn>)
(define (let->combination expr)
  (cons (make-lambda (let-vars expr)
                     (let-body expr))
        (let-inits expr)))


(define (let*->let expr)
  (define (let-reduce associations body)
    (if (null? associations) body
        (make-let (list (car associations))
                  (let-reduce (cdr associations) body))))
  (let-reduce (let-associations expr) (let-body expr)))


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
      (cond ((null? vars) ;; crawl up to the outermost scope
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (let ((v (car vals)))
               (if (eq? v '*unassigned*)
                   (error-log "Unassigned variable" var)
                   v)))
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
      (cond ((null? vars) ;; var free? bind it locally to val
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val) 'ok)
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) ;; variable not bound locally? check outer scope
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
  (info-log 'Set-up)
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
))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (prim-proc)
         (list 'primitive (cdr prim-proc)))
       primitive-procedures))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define (apply-primitive-procedure proc args)
  (let ((primpl (primitive-implementation proc)))
    (debug-log 'Applying 'primitive primpl)
    (let ((result (apply primpl args)))
      (cond ((eq? result '#f) 'false)
            ((eq? result '#t) 'true)
            (else result)))))


;; ************************ Command Line Interface *****************************

(define (repl)
  (define (loop)
    ; read
    (prompt)
    (let ((input (read)))
      (if (not (eof-object? input))
          ; eval
          (let ((output ((analyze input) global-environment)))
            ; print
            (user-print output)
            ; loop
            (loop)))))
  (info-log 'Greetings!)
  (display greet)
  (loop)
  (newline)
  (info-log 'Shut-down)
  (display bye)
  (exit))

(define (user-print obj)
  (cond ((compound-procedure? obj) (write (procedure-body obj)))
        ((primitive-procedure? obj) (write (primitive-implementation obj)))
        (else (write obj))))


(define greet
"  ___ ____________  ___  _____  ___ ______  ___ ____________  ___
 / _ \\| ___ | ___ \\/ _ \\/  __ \\/ _ \\|  _  \\/ _ \\| ___ | ___ \\/ _ \\
/ /_\\ | |_/ | |_/ / /_\\ | /  \\/ /_\\ | | | / /_\\ | |_/ | |_/ / /_\\ \\
|  _  | ___ |    /|  _  | |   |  _  | | | |  _  | ___ |    /|  _  |
| | | | |_/ | |\\ \\| | | | \\__/| | | | |/ /| | | | |_/ | |\\ \\| | | |
\\_| |_\\____/\\_| \\_\\_| |_/\\____\\_| |_|___/ \\_| |_\\____/\\_| \\_\\_| |_/
(c) 2019, Gabriel B. Sant'Anna
Version 0.3.bb\n")

(define (prompt) (display "\n>> "))

(define bye "*** bye! ***\n")


(set-log-verbosity! 'DEBUG) ;; QUIET < ERROR < [WARN] < INFO < @DEBUG

(define global-environment (setup-environment))

(repl)
