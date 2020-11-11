(define-library (match)
  (export
    ;; Tries to match an input list to the given pattern, where a pattern is a
    ;; symbolic expression containing literals (symbols, strings, numbers),
    ;; dotted pairs, simple variables (symbols which start with a question mark,
    ;; eg: ?X) and segment variables [eg: (?* ?X)]
    ;;
    ;; Returns #f when the pattern didn't match (even for partial matches), #t
    ;; when they match with no use of variables or a table containing mappings
    ;; of variables to values in the form of an association list.
    ;;
    ;; Takes as optional third argument an a-list with initial variable bindings.
    pattern-match)

  (import (scheme base)
          (scheme case-lambda)
          (scheme cxr)
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

    (define (segment? pattern)
      (and (pair? pattern)
           (pair? (car pattern))
           (eq? (caar pattern) '?*)))

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
      (cond ((segment? pattern) (match-segment pattern input bindings 0))
            ((and (null? pattern) (null? input)) bindings)
            ((and (pair? pattern) (pair? input))
             (let ((bindings (*pattern-match (car pattern) (car input) bindings)))
               (*pattern-match (cdr pattern) (cdr input) bindings)))
            (else fail-bindings)))

    (define (match-segment pattern input bindings from)
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
                          (match-segment pattern input bindings (+ begin 1))
                          pat-match))))))))

    (define (first-match-index pat input from)
      (let loop ((i 0) (input input))
        (cond ((null? input) (if (literal? pat) #f from))
              ((< i from) (loop (+ i 1) (cdr input))) ; skips some indexes
              ((equal? (car input) pat) i) ; only then check for equality
              (else (loop (+ i 1) (cdr input))))))

    (define fail-bindings '#f)
    (define empty-bindings '())

    (define (look-up key bindings)
      (cond ((assq key bindings) => cdr)
            (else #f)))

    (define (associate var val bindings)
      (alist-cons var val bindings))
  ))
