;;; microKanren implementation as seen in https://www.youtube.com/watch?v=0FwIwewHC3o
;;; Reference paper at https://www.hackernewspapers.com/2016/583-kanren-a-minimal-functional-core-for-relational-programming/

(define (var? term) (number? term))
(define (var term) term)

(define (find term substitution)
  (let ([entry (and (var? term) (assv term substitution))])
    (if entry
        (find (cdr entry) substitution)
        term)))

(define (extend-substitution variable term substitution)
  (cond [(occurs? variable term substitution) #f]
        [else `((,variable . ,term) . ,substitution)]))

(define (occurs? variable term substitution)
  (cond [(var? term) (eqv? variable term)]
        [(pair? term)
         (or (occurs? variable (find (car term) substitution) substitution)
             (occurs? variable (find (cdr term) substitution) substitution))]
        [else #f]))

(define (unify variable term substitution)
  (cond [(eqv? variable term) substitution]
        [(var? variable) (extend-substitution variable term substitution)]
        [(var? term) (unify term variable substitution)]
        [(and (pair? variable) (pair? term))
         (let ([unified (unify (find (car variable) substitution)
                               (find (car term) substitution)
                               substitution)])
          (and unified
               (unify (find (cdr variable) unified)
                      (find (cdr term) unified)
                      unified)))]
        [else #f]))

(define initial-environment '(() . 0))

(define (== lhs rhs)
  (lambda (environment)
    (let ([substitution (car environment)])
      (let ([unified (unify (find lhs substitution)
                            (find rhs substitution)
                            substitution)])
        (if unified (list `(,unified . ,(cdr environment))) '())))))

(define (call/fresh closure)
  (lambda (environment)
    (let ([counter (cdr environment)])
      ((closure (var counter))
       `(,(car environment) . ,(+ 1 counter))))))

(define (disjunction goal1 goal2)
  (lambda (environment)
    ($append (goal1 environment) (goal2 environment))))

(define ($append $1 $2)
  (cond [(null? $1) $2]
        [(promise? $1) (delay ($append $2 (force $1)))]
        [else (cons (car $1) ($append (cdr $1) $2))]))

(define (conjunction goal1 goal2)
  (lambda (environment)
    ($append-map goal2 (goal1 environment))))

(define ($append-map goal $)
  (cond [(null? $) '()]
        [(promise? $) (delay ($append-map goal (force $)))]
        [else ($append (goal (car $)) ($append-map goal (cdr $)))]))

(define-syntax define-relation
  (syntax-rules ()
    [(_ (name . args) goal)
     (define (name . args)
       (lambda (environment)
         (delay (goal environment))))]))

(define (pull $)
  (if (promise? $)
      (pull (force $))
      $))

(define (take n $)
  (cond [(null? $) '()]
        [(and n (zero? (- n 1))) (list (car $))]
        [else (cons (car $)
                    (take (and n (- n 1)) (pull (cdr $))))]))

(define (call/initial-state n goal)
  (take n (pull (goal initial-environment))))


; (define examples '((1 . 2) (2 . cat) (0 . 1)))
; (unify '(1 . 2) '(cat . cat) '()) ; => ((2 . cat) (1 . cat))
; (unify '(1 . 2) '(cat . 1) '())   ; => ((2 . cat) (1 . cat))
; ((call/fresh (lambda (x) (== x 'cat))) initial-environment)

; (define-relation (cats x)
;   (disjunction
;     (== x 'cat)
;     (cats x)))

; (define-relation (turtles x)
;   (disjunction
;     (== x 'turtle)
;     (turtles x)))

; (define r ((call/fresh (lambda (x) (disjunction (turtles x) (cats x)))) initial-environment))
; (take 3 (pull r))

; (define-relation (appendo l s o)
;   (disjunction
;     (conjunction
;       (== l '())
;       (== s 0))
;     (call/fresh
;       (lambda (a)
;         (call/fresh
;           (lambda (d)
;             (conjunction
;               (== l `(,a . ,d))
;               (call/fresh
;                 (lambda (r)
;                   (conjunction
;                     (== `(,a . ,r) o)
;                     (appendo d s r)))))))))))

; (call/initial-state 100
;   (call/fresh
;     (lambda (x)
;       (call/fresh
;         (lambda (y)
;           (call/fresh
;             (lambda (z)
;               (appendo '(a b) '(c d) z))))))))
