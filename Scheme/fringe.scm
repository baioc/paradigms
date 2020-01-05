;; A tree's fringe is the (flattened) sequence of its leaves, eg:
; (same-fringe? '(1 (2 3)) '((1 2) 3)) ; => #t
; (same-fringe? '(1 2 3) '(1 (3 2)))   ; => #f

(define same-fringe? (lambda (tree1 tree2)
  (let ((gen1 (tree->generator tree1))
        (gen2 (tree->generator tree2)))
    (let loop ()
      (let ((leaf1 (gen1))
            (leaf2 (gen2)))
        (if (eqv? leaf1 leaf2)
            (if (null? leaf1) #t (loop))
            #f))))))

(define tree->generator (lambda (tree)
  (let ((caller '*))
    (letrec
      ((generate-leaves
        (lambda ()
          (let loop ((tree tree))
            (cond ((null? tree) 'skip)
                  ((pair? tree)
                    (loop (car tree))
                    (loop (cdr tree)))
                  (else
                    (call/cc
                    (lambda (rest-of-tree)
                      (set! generate-leaves
                            (lambda () (rest-of-tree 'resume)))
                      (caller tree))))))
          (caller '()))))
      (lambda ()
        (call/cc
          (lambda (k)
            (set! caller k)
            (generate-leaves))))))))


; (define-macro coroutine
;   (lambda (x . body)
;     `(letrec ((+local-control-state (lambda (,x) ,@body))
;               (resume (lambda (coroutine value)
;                 (call/cc (lambda (continuation)
;                   (set! +local-control-state continuation)
;                   (coroutine value))))))
;        (lambda (value)
;          (+local-control-state value)))))

; (define same-fringe? (lambda (tree1 tree2)
;   (letrec ((tree-cor-1
;             (make-leaf-gen-coroutine tree1 (lambda (v) (matcher-cor v))))
;            (tree-cor-2
;             (make-leaf-gen-coroutine tree2 (lambda (v) (matcher-cor v))))
;            (matcher-cor
;             (make-matcher-coroutine (lambda (v) (tree-cor-1 v))
;                                     (lambda (v) (tree-cor-2 v)))))
;     (matcher-cor 'start-ball-rolling))))

; (define make-leaf-gen-coroutine (lambda (tree matcher-cor)
;   (coroutine dont-need-an-init-arg
;     (let loop ((tree tree))
;       (cond ((null? tree) 'skip)
;             ((pair? tree)
;               (loop (car tree))
;               (loop (cdr tree)))
;             (else
;               (resume matcher-cor tree))))
;     (resume matcher-cor '()))))

; (define make-matcher-coroutine (lambda (tree-cor-1 tree-cor-2)
;   (coroutine dont-need-an-init-arg
;     (let loop ()
;       (let ((leaf1 (resume tree-cor-1 'get-a-leaf))
;             (leaf2 (resume tree-cor-2 'get-a-leaf)))
;         (if (eqv? leaf1 leaf2)
;             (if (null? leaf1) #t (loop))
;             #f))))))
