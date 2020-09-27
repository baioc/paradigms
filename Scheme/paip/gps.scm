(import (scheme base) (scheme case-lambda))
(import (srfi 1)) ; list library
(import (debug))


;; Represents an action with preconditions and side effects (postconditions).
(define-record-type <action>
  (action name preconds add-list rem-list)
  action?
  (name action-description)
  (preconds action-preconditions)
  (add-list action-additions)
  (rem-list action-remotions))

;; Creates an action object that has the given properties and side effects,
;; such that each effect is a list whose car is + (produced) or - (consumed).
(define (make-action description preconditions effects)
  (let ((produced (filter-map (lambda (e) (and (eq? (car e) '+) (cadr e))) effects))
        (consumed (filter-map (lambda (e) (and (eq? (car e) '-) (cadr e))) effects)))
    (for-each
     (lambda (requirement)
       (unless (contains? preconditions requirement)
         (error "make-action -- missing necessary precondition" requirement)))
     consumed)
    (action description preconditions produced consumed)))

;; Given an initial state (list of symbols) and a list of possible actions,
;; finds a way to achieve all of the given goals (also a list of symbols) and
;; returns either a sequence of <action>s which acomplish those (in case that's
;; possible) or the result of applying the optional last parameter to (f p)
;; where f is the list of goals which couldn't be achieved and p is the sequence
;; of <action>s which would achieve the other goals (a partial result); if this
;; function is not supplied, it defaults to one that always returns #f.
(define general-problem-solver ; aka GPS
  (case-lambda
    ((initial-state goals actions)
     (general-problem-solver initial-state goals actions (lambda (f p) #f)))
    ((initial-state goals actions failwith)
     (make-algorithm
      (achieve-all (make-state initial-state '() '()) goals actions)
      failwith))))

;; Represents a problem's state, including target goals and executed actions.
(define-record-type <state>
  (make-state properties in-progress to-be-done)
  state?
  (properties state-properties)
  (in-progress state-goals)
  (to-be-done state-actions))

(define (make-algorithm result failwith)
  (if (state? result)
      (reverse (state-actions result))
      (failwith (car result) (make-algorithm (cdr result) failwith))))

(define (achieve-all state goals actions)
  (let* ((result
          (fold (lambda (goal state) (or (achieve state goal actions) state)) state goals))
         (failed
          (remove (lambda (desired) (contains? (state-properties result) desired)) goals)))
    (if (null? failed)
        result
        (cons failed result))))

(define (contains? list goal)
  (member goal list))

(define (achieve state goal actions)
  (debug-indent 'GPS (length (state-goals state)))
  (debug-display 'GPS "Goal: ")
  (debug-display 'GPS goal)
  (debug-newline 'GPS)
  (cond
    ;; if we've already achieved the goal, the current state is a solution
    ((contains? (state-properties state) goal) state)
    ;; if we're trying to achieve a goal as a subgoal of itself, halt
    ((contains? (state-goals state) goal) #f)
    ;; otherwise, find the means to achieve the goal and try to commit to any of that
    (else (let ((means (filter (lambda (action) (achieves? action goal)) actions)))
            (any (lambda (way) (try way state goal actions)) means)))))

(define (achieves? action goal)
  (contains? (action-additions action) goal))

;; Try some way to solve a goal from a given state, returning #f if it doesn't
;; work or the updated problem state after applying this solution if it does.
(define (try some-way state goal actions)
  (debug-indent 'GPS (length (state-goals state)))
  (debug-display 'GPS "Considering: ")
  (debug-display 'GPS (action-description some-way))
  (debug-newline 'GPS)
  ;; we must first solve all the required subproblems
  (let* ((subgoals (action-preconditions some-way))
         (subresult (achieve-all (state-considering state goal) subgoals actions)))
    (and (state? subresult)
         (perform some-way subresult)))) ; and only then commit to this way

;; Create a problem state which is just like the given one, but knows we're
;; recursing in order to solve a root goal.
(define (state-considering state goal)
  (make-state (state-properties state)
              (cons goal (state-goals state))
              (state-actions state)))

;; Executes an action over given state, yielding the next one.
(define (perform action state)
  (debug-indent 'GPS (length (state-goals state)))
  (debug-display 'GPS "Execute: ")
  (debug-display 'GPS (action-description action))
  (debug-newline 'GPS)
  (make-state (update-properties (state-properties state)
                                 (action-additions action)
                                 (action-remotions action))
              (state-goals state)
              (cons action (state-actions state))))

(define (update-properties state additions remotions)
  (append
   additions
   (fold
    (lambda (remove state)
      ;; we remove only the first occurence of each property from the list
      (let loop ((state state) (result '()) (removed #f))
        (cond ((null? state) result)
              (removed (loop (cdr state) (cons (car state) result) removed))
              ((equal? (car state) remove) (loop (cdr state) result #t))
              (else (loop (cdr state) (cons (car state) result) removed)))))
    state
    remotions)))


;;; testing

(define (gps state goals actions)
  (define (parse-failed failed partial) (cons '*FAILED* failed))
  (let ((result (general-problem-solver state goals actions parse-failed)))
    (if (eq? (car result) '*FAILED*)
        result
        (cons '*ALGORITHM* (map action-description result)))))

; (start-debugging! 'GPS)

(define school-ops
  (list
   (make-action 'taxi-son-to-school
                '(son-at-home have-money)
                '((+ son-at-school) (- son-at-home) (- have-money)))
   (make-action 'drive-son-to-school
                '(son-at-home car-works)
                '((- son-at-home) (+ son-at-school)))
   (make-action 'shop-installs-battery
                '(car-needs-battery shop-knows-problem shop-has-money)
                '((+ car-works) (- car-needs-battery) (- shop-knows-problem)))
   (make-action 'tell-shop-problem
                '(in-communication-with-shop)
                '((+ shop-knows-problem) (- in-communication-with-shop)))
   (make-action 'telephone-shop
                '(know-phone-number)
                '((+ in-communication-with-shop)))
   (make-action 'ask-phone-number ; may recurse with 'telephone-shop
                '(in-communication-with-shop)
                '((+ know-phone-number)))
   (make-action 'look-up-number
                '(have-phone-book)
                '((+ know-phone-number)))
   (make-action 'give-shop-money
                '(have-money)
                '((- have-money) (+ shop-has-money)))))

(gps
 '(son-at-home car-works)
 '(son-at-school)
 school-ops)

;; this one can be solved but the GPS doesn't find it since it can't backtrack
(gps
 '(son-at-home have-money car-works)
 '(son-at-school have-money)
 school-ops)

(gps
 '(son-at-home car-needs-battery have-money have-phone-book)
 '(son-at-school)
 school-ops)

(gps
 '(son-at-home car-needs-battery have-money)
 '(son-at-school)
 school-ops)

(gps
 '(son-at-home car-needs-battery have-money have-phone-book)
 '(have-money son-at-school)
 school-ops)


(define banana-ops
  (list
   (make-action 'climb-on-chair
                '(chair-at-middle-room at-middle-room on-floor)
                '((+ at-bananas) (+ on-chair) (- at-middle-room) (- on-floor)))
   (make-action 'push-chair-from-door-to-middle-room
                '(chair-at-door at-door)
                '((+ chair-at-middle-room) (+ at-middle-room) (- chair-at-door) (- at-door)))
   (make-action 'walk-from-door-to-middle-room
                '(at-door on-floor)
                '((+ at-middle-room) (- at-door)))
   (make-action 'grasp-bananas
                '(at-bananas empty-handed)
                '((+ has-bananas) (- empty-handed)))
   (make-action 'drop-ball
                '(has-ball)
                '((+ empty-handed) (- has-ball)))
   (make-action 'eat-bananas
                '(has-bananas hungry)
                '((+ empty-handed) (+ not-hungry) (- has-bananas) (- hungry)))))

(gps
 '(at-door on-floor has-ball hungry chair-at-door)
 '(not-hungry)
 banana-ops)


(define (make-maze-action here there)
  (make-action `(move from ,here to ,there)
               `((at ,here))
               `((- (at ,here)) (+ (at ,there)))))

(define (make-maze-corridor pair)
  (list (make-maze-action (first pair) (second pair))
        (make-maze-action (second pair) (first pair))))

;; we need to "manually" generate these since we can't express constraints
(define maze-ops
  (append-map make-maze-corridor
    '(        (1 2)   (2 3)   (3 4)   (10 5)
      (11 6)  (7 8)   (8 9)   (4 9)   (15 10)
      (11 12) (7 12)  (12 13) (9 14)  (20 15)
      (11 16) (16 17) (23 18) (24 19) (19 20)
      (21 22) (17 22) (22 23) (23 24) (20 25))))

(gps '((at 1)) '((at 25)) maze-ops)
