(import (scheme base))
(import (srfi 1) (srfi 95)) ; lists and sorting
(import (debug))

;;; Find a state satisfying a goal, starting with some initial states and
;;; searching their successors as defined by the given combiner function.
(define (tree-search states goal? successors combiner)
  (debug-display 'search "Searching: ")
  (debug-display 'search states)
  (debug-newline 'search)
  (cond ((null? states) #f)
        ((goal? (car states)) (car states))
        (else (tree-search (combiner (successors (car states)) (cdr states))
                            goal?
                            successors
                            combiner))))

(define (depth-first-search start goal? successors)
  (tree-search (list start) goal? successors append))

(define (breadth-first-search start goal? successors)
  (define (prepend a b) (append b a))
  (tree-search (list start) goal? successors prepend))

(define (beam-search start goal? successors cost beam-width)
  (define (sorted-beam-combine new old)
    (let ((sorted (sort (append new old) < cost)))
      (if (> beam-width (length sorted))
          sorted
          (take sorted beam-width))))
  (tree-search (list start) goal? successors sorted-beam-combine))

(define (best-first-search start goal? successors cost)
  (beam-search start goal? successors cost +inf.0))

(define (iterative-widening-search start goal? successors cost width max)
  (if (> width max)
      #f
      (or (beam-search start goal? successors cost width)
          (iterative-widening-search start goal? successors cost (+ width 1) max))))


;; testing

(define (infinite-binary-tree x)
  (list (* 2 x) (+  1 (* 2 x))))

(define (finite-binary-tree n)
  (lambda (x)
    (filter (lambda (node) (<= node n)) (infinite-binary-tree x))))

(define (is value) (lambda (x) (equal? x value)))

(define (diff num)
  (lambda (x) (abs (- x num))))

(start-debugging! 'search)
