(import (scheme base) (scheme case-lambda) (scheme repl) (scheme eval))


(define (horner x pn)
    (if (null? (cdr pn))
        (car pn)
        `(+ ,(car pn) (* ,x ,(horner x (cdr pn))))))

(define (make-polynomial x first-coef . rest-coefs)
  (let ((polynomial (horner x (cons first-coef rest-coefs))))
    (eval
      (list 'lambda (list x) polynomial)
      (interaction-environment))))


(define (differentiate var sexpr)
  (cond ((number? sexpr) 0)
        ((variable? sexpr)
         (if (same-variable? sexpr var) 1 0))
        ((sum? sexpr)
         (make-sum (differentiate var (augend sexpr))
                   (differentiate var (addend sexpr))))
        ((product? sexpr)
         (make-sum
           (make-product (multiplier sexpr)
                         (differentiate var (multiplicand sexpr)))
           (make-product (differentiate var (multiplier sexpr))
                         (multiplicand sexpr))))
        (else
         (error "unknown expression type -- DERIV" sexpr))))

(define (make-derivative var sexpr)
  (let ((deriv (differentiate var sexpr)))
    (eval
      `(lambda (,var) ,deriv)
      (interaction-environment))))


(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum augend addend)
  (cond ((and (number? addend) (number? augend)) (+ augend addend))
        ((and (number? augend) (= augend 0)) addend)
        ((and (number? addend) (= addend 0)) augend)
        (else (list '+ augend addend))))

(define (sum? x)
  (and (list? x) (eq? (car x) '+)))

(define (augend s)
  (cadr s))

(define (addend s)
  (caddr s))

(define (make-product multiplier multiplicand)
  (define (=number? sexpr num)
    (and (number? sexpr) (= sexpr num)))
  (cond ((or (=number? multiplier 0) (=number? multiplicand 0))
          0)
        ((and (number? multiplier) (number? multiplicand))
          (* multiplier multiplicand))
        ((=number? multiplier 1) multiplicand)
        ((=number? multiplicand 1) multiplier)
        (else (list '* multiplier multiplicand))))

(define (product? x)
  (and (list? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))


(define coefs '(-1 -1 1))

(define p
  (apply horner
         (list 'x coefs))) ;; p = x^2 - x - 1

(define f
  (apply make-polynomial
         (cons 'x coefs))) ;; f(x) = p(x)

(define df
  (make-derivative 'x p))  ;; df(x) = p'(x) = 2x - 1


(define (curry args body . rest)
  (cond ((null? args)
         `(lambda () ,body ,@rest))
        ((null? (cdr args))
         `(lambda (,(car args)) ,body ,@rest))
        (else
         `(lambda (,(car args)) ,(apply curry `(,(cdr args) ,body ,@rest))))))

(define car-cdring
  (case-lambda ; takes an optional eqv predicate to find key, default is `eqv?`
    ((lst key) (car-cdring lst key eqv?))
    ((lst key eqv?)
     ;; returns #f when key not found, or a list of 'cars and 'cdrs
     (define (sexp-tree-path tree key)
       (cond ((null? tree) #f)
             ((not (pair? tree)) (if (eqv? tree key) '() #f))
             ((sexp-tree-path (car tree) key) => (lambda (path) (cons 'car path)))
             ((sexp-tree-path (cdr tree) key) => (lambda (path) (cons 'cdr path)))
             (else #f)))

     ;; converts something like '(cdr cdr car) to '(car (cdr (cdr arg)))
     (define (path->car-cdring path arg)
       (if (null? path)
           arg
           (path->car-cdring (cdr path) (list (car path) arg))))

     (let ((path (sexp-tree-path lst key)))
       (and path
            `(lambda (lst) ,(path->car-cdring path 'lst)))))))
