;; equivalent to the sigma notation \sum_{i=a}^{b} term_i
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))


;; cond works like a switch-case but with conditions & consequences
;; else works only with cond and always evaluates to true, like a default case
(define (flatten sequence)
  (cond ((null? sequence) '())
        ((list? (car sequence))
          (append (flatten (car sequence))
                  (flatten (cdr sequence))))
        (else (cons (car sequence)
                    (flatten (cdr sequence))))))

;; cmpfn is the function that checks order between two elements in the sequence
;; possible examples are '<' '<=' '>' '>=' ... 'string<?' and so on
(define (sorted? seq cmpfn)
  (or (<= (length seq) 1)
      (and (cmpfn (car seq) (cadr seq))
           (sorted? (cdr seq) cmpfn))))

;; tighter recursion with map and apply
;; function override: the other 'flatten' can't be called now
(define (flatten seq)
  (if (not (list? seq)) (list seq)
      (apply append (map flatten seq))))


;; lambda + let use case
;; using let with ps-rest avoids a duplicate recursive call
(define (power-set set)
  (if (null? set) '(())
      (let ((ps-rest (power-set (cdr set))))
        (append ps-rest
                (map (lambda (subset) (cons (car set) subset))
                     ps-rest)))))


;; needed in permute
(define (remove x ls)
  (if (null? ls) '()
      (if (eqv? x (car ls))
          (remove x (cdr ls))
          (cons (car ls) (remove x (cdr ls))))))

;; (map (lambda (x) (func)) list) <=> [func(x) for x in list]
(define (permute items)
  (if (null? items) '(())
      (apply append
             (map (lambda (elem) ;; for each elem in items
                  (map (lambda (permutation)
                         ;; for each "rest" permutation, add in the removed elem
                         (cons elem permutation))
                       ;; remove elem from items and permute the rest
                       (permute (remove elem items))))
             items))))


(define (map proc seq)
  (if (null? seq) seq
      (cons (proc (car seq))
            (map proc (cdr seq)))))


(define test '(10 (1 2) ((3) () 4)))
