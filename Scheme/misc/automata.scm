(import (scheme base))


;; an interpreter for an automata DSL
(define (run machine init-state input)
  (let walker ((state init-state) (input input))
    (cond ((null? input) #t)
          (else
           (let ((in (car input))
                 (transitions (cdr (assv state machine))))
             (let ((new-state (assv in transitions)))
               (and new-state
                    (walker (cadr new-state) (cdr input)))))))))

(define cxr-description
  '((init (c more))
    (more (a more)
          (d more)
          (r end))
    (end)))
(run cxr-description 'init '(c a d a d d r)) ; => #t
(run cxr-description 'init '(c a d a d d r r)) ; => #f


;; a macro that compiles an automaton into mutually recursive procedures
(define-syntax automaton
  (syntax-rules (:)
    ((_ init
       (state : transitions ...)
       ...)
     (let-syntax
      ((process-state
         (syntax-rules (-> accept)
           ((_ accept)
            (lambda (input)
              (null? input)))
           ((_ (symbol -> next-state) (... ...)) ; "ellipsis quotation"
            (lambda (input)
              (cond ((null? input) #f)
                    (else
                     (case (car input)
                       ((symbol) (next-state (cdr input)))
                       (... ...)
                       (else #f)))))))))
        (letrec ((state (process-state transitions ...))
                 ...)
          init)))))

(define cxr
  (automaton init
    (init : (c -> more))
    (more : (a -> more)
            (d -> more)
            (r -> end))
    (end : accept)))

(cxr '(c a d a d d r)) ; => #t
(cxr '(c a d a d d r r)) ; => #f
(cxr '(c a d r)) ; => #t
(cxr '(c a d a)) ; => #f
(cxr '(c a d a r)) ; => #t
(cxr '(c a d a r r)) ; => #f
