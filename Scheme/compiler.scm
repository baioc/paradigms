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


(define (compile-sequence seq target linkage)
  (if (last-expr? seq)
      (compile (first-expr seq) target linkage)
      (preserving '(env continue)
        (compile (first-expr seq) target 'next)
        (compile-sequence (rest-exprs seq) target linkage))))

(define (compile-lambda expr target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage lambda-linkage
            (make-instruction-sequence '(env) (list target)
              `((assign ,target
                        (op make-compiled-procedure)
                        (label ,proc-entry)
                        (reg env)))))
          (compile-lambda-body expr proc-entry))
        after-lambda))))

(define (compile-lambda-body expr proc-entry)
  (let ((formals (lambda-parameters expr)))
    (append-instruction-sequences
      (make-instruction-sequence '(env proc argl) '(env)
        `(,proc-entry
           (assign env (op compiled-procedure-env) (reg proc))
           (assign env
                   (op extend-environment)
                   (const ,formals)
                   (reg argl)
                   (reg env))))
      (compile-sequence (lambda-body expr) 'val 'return))))

(define (compile-application expr target linkage)
  (let ((proc-code (compile (operator expr) 'proc 'next))
        (operand-codes (map (lambda (operand) (compile operand 'val 'next))
                            (operands expr))))
    (preserving '(env continue)
      proc-code
      (preserving '(proc continue)
        (construct-arglist operand-codes)
        (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
          '((assign argl (const ()))))
        (let ((code-to-get-last-arg
                (append-instruction-sequences
                  (car operand-codes)
                  (make-instruction-sequence '(val) '(argl)
                    '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                code-to-get-last-arg
                (code-to-get-rest-args (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl)
            (car operand-codes)
            (make-instruction-sequence '(val argl) '(argl)
              '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
          code-for-next-arg
          (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
        (make-instruction-sequence '(proc) '()
          `((test (op primitive-procedure?) (reg proc))
            (branch (label ,primitive-branch))))
        (parallel-instruction-sequences
          (append-instruction-sequences
            compiled-branch
            (compile-proc-appl target compiled-linkage))
          (append-instruction-sequences
            primitive-branch
            (end-with-linkage linkage
              (make-instruction-sequence '(proc argl) (list target)
                `((assign ,target
                    (op apply-primitive-procedure)
                    (reg proc)
                    (reg argl)))))))
        after-call))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry) (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
             `((assign continue (label ,proc-return))
               (assign val (op compiled-procedure-entry) (reg proc))
               (goto (reg val))
               ,proc-return
               (assign ,target (reg val))
               (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
           '((assign val (op compiled-procedure-entry) (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))


(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union (registers-needed seq1)
                  (list-difference (registers-needed seq2)
                                   (registers-modified seq1)))
      (list-union (registers-modified seq1)
                  (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs) (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
              (make-instruction-sequence
                (list-union (list first-reg) (registers-needed seq1))
                (list-difference (registers-modified seq1) (list first-reg))
                (append `((save ,first-reg))
                        (statements seq1)
                        `((restore ,first-reg))))
              seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1)
                (registers-needed seq2))
    (list-union (registers-modified seq1)
                (registers-modified seq2))
    (append (statements seq1) (statements seq2))))


(define all-regs '(env proc val argl continue))

(define (lambda-parameters expr)
  (cadr expr))

(define (lambda-body expr)
  (cddr expr))

(define (first-expr exprs)
  (car exprs))

(define (rest-exprs exprs)
  (cdr exprs))

(define (last-expr? exprs)
  (null? (cdr exprs)))

(define (if-predicate sexpr)
  (cadr sexpr))

(define (if-consequent sexpr)
  (caddr sexpr))

(define (if-alternative sexpr)
  (if (null? (cdddr sexpr)) 'false ;; unspecified
      (cadddr sexpr)))

(define (assignment-variable sexpr)
  (cadr sexpr))

(define (assignment-value sexpr)
  (caddr sexpr))

(define (text-of-quotation sexpr)
  (cadr sexpr))

(define (clauses cexpr)
  (cdr cexpr))

(define (begin-actions sexpr)
    (cdr sexpr))

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
        (cons 'lambda
              (cons (cdr definiendum) ;; formal arguments
                    definiens))))) ;; body

(define (operator sexpr)
  (car sexpr))

(define (operands sexpr)
  (cdr sexpr))

(define (tagged-list? sexpr tag)
  (and (pair? sexpr)
        (eq? (car sexpr) tag)))

(define (application? sexpr)
  (pair? sexpr)) ;; untagged list

(define (cond? sexpr)
  (tagged-list? sexpr 'cond))

(define (begin? sexpr)
  (tagged-list? sexpr 'begin))

(define (lambda? sexpr)
  (tagged-list? sexpr 'lambda))

(define (if? sexpr)
  (tagged-list? sexpr 'if))

(define (definition? sexpr)
  (tagged-list? sexpr 'define))

(define (assignment? sexpr)
  (tagged-list? sexpr 'set!))

(define (quoted? sexpr)
  (tagged-list? sexpr 'quote))

(define (self-evaluating? expr)
  (or (number? expr)
      (string? expr)))

(define (variable? sexpr)
  (or (symbol? sexpr)
      (boolean? sexpr)))


;; eg:
; (compile
;  '(define (gcd a b)
;     (if (= b 0) a
;         (gcd b (modulo a b))))
;  'val
;  'next)
