
(define (match pattern sexpr dictionary)
  (display "MATCHING:\n")
  (display pattern) (newline)
  (display sexpr) (newline)
  (display dictionary) (newline)
  (cond ((eq? dictionary 'failed) dictionary)
        ((atom? pattern)
         (if (atom? sexpr)
             (if (eq? pattern sexpr)
                 dictionary
                 (begin
                   (display "FAILED DIFF-VAR:\n")
                   (display pattern) (newline)
                   (display sexpr) (newline)
                   (display dictionary) (newline)
                   'failed))
             (begin
                   (display "FAILED PAT-EXHAUST:\n")
                   (display pattern) (newline)
                   (display sexpr) (newline)
                   (display dictionary) (newline)
                   'failed)))
        ((arbitrary-constant? pattern)
         (if (constant? sexpr)
             (extend-dictionary pattern sexpr dictionary))
             (begin
                   (display "FAILED CONST:\n")
                   (display pattern) (newline)
                   (display sexpr) (newline)
                   (display dictionary) (newline)
                   'failed))
        ((arbitrary-variable? pattern)
         (if (variable? sexpr)
             (extend-dictionary pattern sexpr dictionary))
             (begin
                   (display "FAILED VAR:\n")
                   (display pattern) (newline)
                   (display sexpr) (newline)
                   (display dictionary) (newline)
                   'failed))
        ((arbitrary-expression? pattern)
         (extend-dictionary pattern sexpr dictionary))
        ((atom? sexpr) (begin
                   (display "FAILED EXP-EXHAUST:\n")
                   (display pattern) (newline)
                   (display sexpr) (newline)
                   (display dictionary) (newline)
                   'failed))
        (else (match (cdr pattern)
                     (cdr sexpr)
                     (match (car pattern)
                            (car sexpr)
                            dictionary)))))


(define (instantiate skeleton dictionary)
  (let loop ((skel skeleton))
    (cond ((atom? skel) skel)
          ((skeleton-evaluation? skel)
           (evaluate (eval-expr skel) dictionary))
          (else (cons (loop (car skel))
                      (loop (cdr skel)))))))

(define (evaluate sexpr dictionary)
  (if (atom? sexpr)
      (lookup sexpr dictionary)
      (apply (eval (lookup (car sexpr) dictionary))
             (map (lambda (arg) (evaluate arg dictionary))
                  (cdr sexpr)))))


(define (simplifier rule-definitions)
  (define (simplify-expr sexpr)
    (try-rules (if (list? sexpr)
                   (map simplify-expr sexpr)
                   sexpr)))
  (define (try-rules expr)
    (define (scan rules)
      (if (null? rules) expr
          (let ((dict (match (pattern (car rules)) expr (empty-dictionary))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-expr (instantiate (skeleton (car rules)) dict))))))
    (scan rule-definitions))
  simplify-expr)


(define (empty-dictionary) '())

(define (extend-dictionary pat dat dict)
  (let* ((name (variable-name pat))
         (val (assq name dict)))
    (cond ((not val)
           (cons (list name dat) dict))
          ((eq? (cadr val) dat) dict)
          (else 'failed))))

(define (lookup var dict)
  (let ((val (assq var dict)))
    (if (null? val) var (cadr val))))


(define (atom? sexpr)
  (not (pair? sexpr)))

(define variable-name cadr)

(define pattern car)

(define skeleton cadr)

(define (arbitrary-constant? sexpr)
  (eq? (car sexpr) '?const))

(define (constant? sexpr)
  (number? sexpr))

(define (arbitrary-variable? sexpr)
  (eq? (car sexpr) '?var))

(define (variable? sexpr)
  (symbol? sexpr))

(define (arbitrary-expression? sexpr)
  (eq? (car sexpr) '?))

(define (skeleton-evaluation? skel)
  (eq? (car skel) ':))

(define eval-expr cadr)


(define derivative-rules
  '(
    ( (dd (?const c) (?var x))         0 )
    ( (dd (?var x) (?var x))           1 )
    ; ( (dd (?var y) (?var x))           0 )
    ; ( (dd (+ (? u) (? v)) (?var x))
    ;   (+ (dd (: u) (: x))
    ;      (dd (: v) (: x)))               )
    ; ( (dd (- (? u) (? v)) (?var x))
    ;   (- (dd (: u) (: x))
    ;      (dd (: v) (: x)))               )
    ; ( (dd (* (? u) (? v)) (?var x))
    ;   (+ (* (: u) (dd (: v) (: x)))
    ;      (* (dd (: u) (: x)) (: v)))     )
    ; ( (dd (/ (? u) (? v)) (?var x))
    ;   (/ (- (* (dd (: u) (: x)) (: v))
    ;         (* (dd (: v) (: x)) (: u)))
    ;      (^ (: v) 2))                    )
    ; ( (dd (^ (? u) (?const n)) (?var x))
    ;   (* (* (: n) (^ (: u) (- (: n) 1)))
    ;      (dd (: u) (: x)))               )
  ))

(define dsimp (simplifier derivative-rules))

(define foo
  '(+ (+ (* a (* x x))
         (* b x))
      c))

; (dsimp `(dd ,foo x))
