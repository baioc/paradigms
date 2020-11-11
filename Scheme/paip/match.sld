(define-library (match)
  (export
    ;;; Tries to match an input list to the given pattern.
    ;;;
    ;;; Patterns can be a symbolic expression containing literals (symbols,
    ;;; strings, numbers), dotted lists, simple variables (symbols which start
    ;;; with a question mark, eg: ?X) or complex variable patterns:
    ;;;   (?and patterns ...)  ; => match only if all patterns match
    ;;;   (?or patterns ...)   ; => match when at least one pattern matches
    ;;;   (?not patterns ...)  ; => match only if no patterns match
    ;;;   (?is ?var predicate) ; => match only if a predicate holds for a variable
    ;;;   (?* ?var)            ; => match variable to zero or more symbols
    ;;;   (?+ ?var)            ; => match variable to one or more symbols
    ;;;   (?? ?var)            ; => match variable to one or zero symbols
    ;;;   (?if code)           ; => match only if some scheme code (can use
    ;;;                        ;    previous symbolic variables) returns true
    ;;;
    ;;; Returns #f when the pattern didn't match (even for partial matches), #t
    ;;; when they match with no use of variables or a table containing mappings
    ;;; of variables to values in the form of an association list.
    ;;;
    ;;; Takes as optional third argument an a-list with initial variable bindings.
    pattern-match)

  (import (scheme base)
          (scheme case-lambda)
          (scheme cxr)
          (scheme eval)
          (srfi 125) (srfi 128) ; hash tables and comparators
          (srfi 1))

  (begin
    (define pattern-match
      (case-lambda
        ((pattern input) (pattern-match pattern input '()))
        ((pattern input bindings)
         (let ((result (*pattern-match pattern input bindings)))
           (cond ((eq? result fail-bindings) #f)
                 ((eq? result empty-bindings) #t)
                 (else result))))))

    (define (literal? obj)
      (or (and (symbol? obj) (not (variable? obj)))
          (string? obj)
          (number? obj)))

    (define (variable? obj)
      (and (symbol? obj)
           (char=? #\? (string-ref (symbol->string obj) 0))))

    ;; actual implementation
    (define (*pattern-match pattern input bindings)
      (cond ((eq? bindings fail-bindings) fail-bindings)
            ((literal? pattern) (match-literal pattern input bindings))
            ((variable? pattern) (match-variable pattern input bindings))
            ((list? pattern) (match-list pattern input bindings))
            (else (error "pattern-match -- unexpected pattern format" pattern))))

    (define (match-literal pattern input bindings)
      (if (equal? pattern input) bindings fail-bindings))

    (define (match-variable variable value bindings)
      (let ((present (look-up variable bindings)))
        (cond ((not present) (associate variable value bindings))
              ((equal? value present) bindings)
              (else fail-bindings))))

    (define (match-list pattern input bindings)
      (cond ((segment-pattern? pattern) (match-segment pattern input bindings))
            ((single-pattern? pattern) (match-single pattern input bindings))
            ((and (null? pattern) (null? input)) bindings)
            ((and (pair? pattern) (pair? input))
             (let ((bindings (*pattern-match (car pattern) (car input) bindings)))
               (*pattern-match (cdr pattern) (cdr input) bindings)))
            (else fail-bindings)))

    (define (segment-pattern? pattern)
      (and (pair? pattern)
           (pair? (car pattern))
           (segment-matcher (caar pattern))))

    (define (single-pattern? pattern)
      (and (pair? pattern)
           (single-matcher (car pattern))))

    (define (segment-matcher identifier)
      (and (symbol? identifier)
           (hash-table-ref/default *segment-matchers* identifier #f)))

    (define (single-matcher identifier)
      (and (symbol? identifier)
           (hash-table-ref/default *single-matchers* identifier #f)))

    (define (match-segment pattern input bindings)
      ((segment-matcher (caar pattern)) pattern input bindings))

    (define (match-single pattern input bindings)
      ((single-matcher (car pattern)) (cdr pattern) input bindings))

    ;; match all
    (define (match-and patterns input bindings)
      (cond ((eq? bindings fail-bindings) fail-bindings)
            ((null? patterns) bindings)
            (else (let ((first (*pattern-match (car patterns) input bindings)))
                    (match-and (cdr patterns) input first)))))

    ;; match any
    (define (match-or patterns input bindings)
      (if (null? patterns)
          fail-bindings
          (let ((new-bindings (*pattern-match (car patterns) input bindings)))
            (if (eq? first fail-bindings)
                (match-or (cdr patterns) input bindings)
                new-bindings))))

    ;; match none
    (define (match-not patterns input bindings)
      (if (eq? fail-bindings (match-or patterns input bindings))
          bindings
          fail-bindings))

    ;; match if predicate returns true ON THE INPUT
    (define (match-is var-and-pred input bindings)
      (let ((var (car var-and-pred))
            (pred (cadr var-and-pred)))
        (let ((var-match (*pattern-match var input bindings))
              (pred? (eval pred (environment '(scheme base)))))
          (if (or (eq? var-match fail-bindings)
                  (not (pred? input)))
              fail-bindings
              var-match))))

    (define (match-segment* pattern input bindings)
      (match-segment-from pattern input bindings 0))

    (define (match-segment+ pattern input bindings)
      (match-segment-from pattern input bindings 1))

    (define (match-segment? pattern input bindings)
      (let ((var (cadar pattern))
            (pat (cdr pattern)))
        (or (*pattern-match (cons var pat) input bindings)
            (*pattern-match pat input bindings))))

    (define (match-segmentif pattern input bindings)
      (let ((code (cadar pattern))
            (vars (bindings->lets bindings)))
        (if (eval `(let ,vars ,code) (environment '(scheme base)))
            (*pattern-match (cdr pattern) input bindings)
            fail-bindings)))

    (define (match-segment-from pattern input bindings from)
      ;; segment pattern looks like ((?* var) . pat)
      (let ((var (cadar pattern))
            (pat (cdr pattern)))
        ;; when a segment is at tail position, match whole input to a variable
        (if (null? pat)
            (match-variable var input bindings)
            ;; otherwise, first get the index of the first partial tail match
            (let ((begin (first-match-index (car pat) input from)))
              (if (not begin)
                  fail-bindings
                  ;; then partition the input at that index and match separately
                  (let-values (((var-input pat-input) (split-at input begin)))
                    (let* ((var-match (match-variable var var-input bindings))
                           (pat-match (*pattern-match pat pat-input var-match)))
                      ;; when not found, iteratively increase searched length
                      (if (eq? pat-match fail-bindings)
                          (match-segment-from pattern input bindings (+ begin 1))
                          pat-match))))))))

    (define (first-match-index pat input from)
      (let loop ((i 0) (input input))
        (cond ((null? input) (if (or (literal? pat) (> from i)) #f from))
              ((< i from) (loop (+ i 1) (cdr input))) ; skips some indexes
              ((equal? (car input) pat) i) ; only then check for equality
              (else (loop (+ i 1) (cdr input))))))

    (define *single-matchers*
      (hash-table (make-eq-comparator)
        '?and match-and
        '?or  match-or
        '?not match-not
        '?is  match-is))

    (define *segment-matchers*
      (hash-table (make-eq-comparator)
        '?*  match-segment*
        '?+  match-segment+
        '??  match-segment?
        '?if match-segmentif))

    ;; we use a-lists for bindings only because they're purely functional...
    (define empty-bindings '())

    (define (look-up key bindings)
      (cond ((assq key bindings) => cdr)
            (else #f)))

    (define (associate var val bindings)
      (alist-cons var val bindings))

    (define (bindings->lets bindings)
      (map
       (lambda (binding)
         (let ((var (car binding)) (val (cdr binding)))
           (list var val)))
       bindings))

    (define fail-bindings #f)))
