#:INFO Set-up 
#:INFO Greetings! 
  ___ ____________  ___  _____  ___ ______  ___ ____________  ___
 / _ \| ___ | ___ \/ _ \/  __ \/ _ \|  _  \/ _ \| ___ | ___ \/ _ \
/ /_\ | |_/ | |_/ / /_\ | /  \/ /_\ | | | / /_\ | |_/ | |_/ / /_\ \
|  _  | ___ |    /|  _  | |   |  _  | | | |  _  | ___ |    /|  _  |
| | | | |_/ | |\ \| | | | \__/| | | | |/ /| | | | |_/ | |\ \| | | |
\_| |_\____/\_| \_\_| |_/\____\_| |_|___/ \_| |_\____/\_| \_\_| |_/
(c) 2019, Gabriel B. Sant'Anna
Version 0.3.bb

>> #:DEBUG Analyzing 42 
#:INFO Atom 42 
42
>> #:DEBUG Analyzing 0.03 
#:INFO Atom 0.03 
0.03
>> #:DEBUG Analyzing 1.2e-13 
#:INFO Atom 1.2e-13 
1.2e-13
>> #:DEBUG Analyzing 1e-90 
#:INFO Atom 1e-90 
1e-90
>> #:DEBUG Analyzing 3/5 
#:INFO Atom 3/5 
3/5
>> #:DEBUG Analyzing 3+4i 
#:INFO Atom 3+4i 
3+4i
>> #:DEBUG Analyzing #\c 
#:INFO Atom #\c 
#\c
>> #:DEBUG Analyzing "this is a string" 
#:INFO Atom "this is a string" 
"this is a string"
>> #:DEBUG Analyzing (quote a) 
#:DEBUG Fetching syntax rules for quote 
a
>> #:DEBUG Analyzing false 
#:INFO Looking up false 
false
>> #:DEBUG Analyzing true 
#:INFO Looking up true 
true
>> #:DEBUG Analyzing x 
#:INFO Looking up x 
WARNING: Unbound variable x 
#<unspecified>
>> #:DEBUG Analyzing (define x 99) 
#:DEBUG Fetching syntax rules for define 
#:DEBUG Analyzing 99 
#:INFO Atom 99 
#:INFO Define x : 99 
ok
>> #:DEBUG Analyzing x 
#:INFO Looking up x 
99
>> #:DEBUG Analyzing (set! x -1) 
#:DEBUG Fetching syntax rules for set! 
#:DEBUG Analyzing -1 
#:INFO Atom -1 
#:INFO Assign x = -1 
ok
>> #:DEBUG Analyzing x 
#:INFO Looking up x 
-1
>> #:DEBUG Analyzing #t 
#:INFO Looking up #t 
WARNING: Unbound variable #t 
#<unspecified>
>> #:DEBUG Analyzing #f 
#:INFO Looking up #f 
WARNING: Unbound variable #f 
#<unspecified>
>> #:DEBUG Analyzing eq? 
#:INFO Looking up eq? 
#<procedure (scheme#eq? x y)>
>> #:DEBUG Analyzing (eq? false (quote false)) 
#:INFO Analyzing application (eq? false (quote false)) 
#:DEBUG Analyzing eq? 
#:DEBUG Analyzing false 
#:DEBUG Analyzing (quote false) 
#:DEBUG Fetching syntax rules for quote 
#:INFO Executing (eq? false (quote false)) 
#:INFO Looking up eq? 
#:INFO Looking up false 
#:DEBUG Applying primitive #<procedure (scheme#eq? x y)> 
true
>> #:DEBUG Analyzing (eq? (quote lambda) (quote λ)) 
#:INFO Analyzing application (eq? (quote lambda) (quote λ)) 
#:DEBUG Analyzing eq? 
#:DEBUG Analyzing (quote lambda) 
#:DEBUG Fetching syntax rules for quote 
#:DEBUG Analyzing (quote λ) 
#:DEBUG Fetching syntax rules for quote 
#:INFO Executing (eq? (quote lambda) (quote λ)) 
#:INFO Looking up eq? 
#:DEBUG Applying primitive #<procedure (scheme#eq? x y)> 
true
>> #:DEBUG Analyzing (+ 1 2 3 4) 
#:INFO Analyzing application (+ 1 2 3 4) 
#:DEBUG Analyzing + 
#:DEBUG Analyzing 1 
#:INFO Atom 1 
#:DEBUG Analyzing 2 
#:INFO Atom 2 
#:DEBUG Analyzing 3 
#:INFO Atom 3 
#:DEBUG Analyzing 4 
#:INFO Atom 4 
#:INFO Executing (+ 1 2 3 4) 
#:INFO Looking up + 
#:DEBUG Applying primitive #<procedure C_plus> 
10
>> #:DEBUG Analyzing (define (test b) (if b (display "#TRUE\n") (display "#FALSE\n"))) 
#:DEBUG Fetching syntax rules for define 
#:DEBUG Making lambda (b) -> ((if b (display "#TRUE\n") (display "#FALSE\n"))) 
#:DEBUG Analyzing (lambda (b) (if b (display "#TRUE\n") (display "#FALSE\n"))) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing (if b (display "#TRUE\n") (display "#FALSE\n")) 
#:DEBUG Fetching syntax rules for if 
#:DEBUG Analyzing b 
#:DEBUG Analyzing (display "#TRUE\n") 
#:INFO Analyzing application (display "#TRUE\n") 
#:DEBUG Analyzing display 
#:DEBUG Analyzing "#TRUE\n" 
#:INFO Atom "#TRUE\n" 
#:DEBUG Analyzing (display "#FALSE\n") 
#:INFO Analyzing application (display "#FALSE\n") 
#:DEBUG Analyzing display 
#:DEBUG Analyzing "#FALSE\n" 
#:INFO Atom "#FALSE\n" 
#:INFO Define test : (lambda (b) (if b (display "#TRUE\n") (display "#FALSE\n"))) 
ok
>> #:DEBUG Analyzing (test (quote #f)) 
#:INFO Analyzing application (test (quote #f)) 
#:DEBUG Analyzing test 
#:DEBUG Analyzing (quote #f) 
#:DEBUG Fetching syntax rules for quote 
#:INFO Executing (test (quote #f)) 
#:INFO Looking up test 
#:INFO Conditionally b ? (display "#TRUE\n") : (display "#FALSE\n") 
#:INFO Looking up b 
#:INFO Executing (display "#TRUE\n") 
#:INFO Looking up display 
#:DEBUG Applying primitive #<procedure (scheme#display x . rest)> 
#TRUE
#<unspecified>
>> #:DEBUG Analyzing (test false) 
#:INFO Analyzing application (test false) 
#:DEBUG Analyzing test 
#:DEBUG Analyzing false 
#:INFO Executing (test false) 
#:INFO Looking up test 
#:INFO Looking up false 
#:INFO Conditionally b ? (display "#TRUE\n") : (display "#FALSE\n") 
#:INFO Looking up b 
#:INFO Executing (display "#FALSE\n") 
#:INFO Looking up display 
#:DEBUG Applying primitive #<procedure (scheme#display x . rest)> 
#FALSE
#<unspecified>
>> #:DEBUG Analyzing (define l0 (lambda b b)) 
#:DEBUG Fetching syntax rules for define 
#:DEBUG Analyzing (lambda b b) 
#:DEBUG Fetching syntax rules for lambda 
WARNING: Improper formal argument list b 
#:DEBUG Analyzing b 
#:INFO Define l0 : (lambda b b) 
ok
>> #:DEBUG Analyzing (l0 0) 
#:INFO Analyzing application (l0 0) 
#:DEBUG Analyzing l0 
#:DEBUG Analyzing 0 
#:INFO Atom 0 
#:INFO Executing (l0 0) 
#:INFO Looking up l0 
#:INFO Looking up b 
0
>> #:DEBUG Analyzing (define l1 (λ (b) b)) 
#:DEBUG Fetching syntax rules for define 
#:DEBUG Analyzing (λ (b) b) 
#:DEBUG Fetching syntax rules for λ 
#:DEBUG Analyzing b 
#:INFO Define l1 : (λ (b) b) 
ok
>> #:DEBUG Analyzing (l1 1) 
#:INFO Analyzing application (l1 1) 
#:DEBUG Analyzing l1 
#:DEBUG Analyzing 1 
#:INFO Atom 1 
#:INFO Executing (l1 1) 
#:INFO Looking up l1 
#:INFO Looking up b 
1
>> #:DEBUG Analyzing (define (l2 b) b) 
#:DEBUG Fetching syntax rules for define 
#:DEBUG Making lambda (b) -> (b) 
#:DEBUG Analyzing (lambda (b) b) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing b 
#:INFO Define l2 : (lambda (b) b) 
ok
>> #:DEBUG Analyzing (l2 2) 
#:INFO Analyzing application (l2 2) 
#:DEBUG Analyzing l2 
#:DEBUG Analyzing 2 
#:INFO Atom 2 
#:INFO Executing (l2 2) 
#:INFO Looking up l2 
#:INFO Looking up b 
2
>> #:DEBUG Analyzing (define l3 (lambda (b) (b))) 
#:DEBUG Fetching syntax rules for define 
#:DEBUG Analyzing (lambda (b) (b)) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing (b) 
#:INFO Analyzing application (b) 
#:DEBUG Analyzing b 
#:INFO Define l3 : (lambda (b) (b)) 
ok
>> #:DEBUG Analyzing (l3 newline) 
#:INFO Analyzing application (l3 newline) 
#:DEBUG Analyzing l3 
#:DEBUG Analyzing newline 
#:INFO Executing (l3 newline) 
#:INFO Looking up l3 
#:INFO Looking up newline 
#:INFO Executing (b) 
#:INFO Looking up b 
#:DEBUG Applying primitive #<procedure (scheme#newline . rest)> 

#<unspecified>
>> #:DEBUG Analyzing (define (l4) (l3 (l2 newline)) (l3 (l1 newline)) (1- -1)) 
#:DEBUG Fetching syntax rules for define 
#:DEBUG Making lambda () -> ((l3 (l2 newline)) (l3 (l1 newline)) (1- -1)) 
#:DEBUG Analyzing (lambda () (l3 (l2 newline)) (l3 (l1 newline)) (1- -1)) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing (l3 (l2 newline)) 
#:INFO Analyzing application (l3 (l2 newline)) 
#:DEBUG Analyzing l3 
#:DEBUG Analyzing (l2 newline) 
#:INFO Analyzing application (l2 newline) 
#:DEBUG Analyzing l2 
#:DEBUG Analyzing newline 
#:DEBUG Analyzing (l3 (l1 newline)) 
#:INFO Analyzing application (l3 (l1 newline)) 
#:DEBUG Analyzing l3 
#:DEBUG Analyzing (l1 newline) 
#:INFO Analyzing application (l1 newline) 
#:DEBUG Analyzing l1 
#:DEBUG Analyzing newline 
#:DEBUG Analyzing (1- -1) 
#:INFO Analyzing application (1- -1) 
#:DEBUG Analyzing 1- 
#:DEBUG Analyzing -1 
#:INFO Atom -1 
#:INFO Define l4 : (lambda () (l3 (l2 newline)) (l3 (l1 newline)) (1- -1)) 
ok
>> #:DEBUG Analyzing (begin (l4) (quote hey)) 
#:DEBUG Fetching syntax rules for begin 
#:DEBUG Analyzing (l4) 
#:INFO Analyzing application (l4) 
#:DEBUG Analyzing l4 
#:DEBUG Analyzing (quote hey) 
#:DEBUG Fetching syntax rules for quote 
#:INFO Executing (l4) 
#:INFO Looking up l4 
#:INFO Executing (l3 (l2 newline)) 
#:INFO Looking up l3 
#:INFO Executing (l2 newline) 
#:INFO Looking up l2 
#:INFO Looking up newline 
#:INFO Looking up b 
#:INFO Executing (b) 
#:INFO Looking up b 
#:DEBUG Applying primitive #<procedure (scheme#newline . rest)> 

#:INFO Executing (l3 (l1 newline)) 
#:INFO Looking up l3 
#:INFO Executing (l1 newline) 
#:INFO Looking up l1 
#:INFO Looking up newline 
#:INFO Looking up b 
#:INFO Executing (b) 
#:INFO Looking up b 
#:DEBUG Applying primitive #<procedure (scheme#newline . rest)> 

#:INFO Executing (1- -1) 
#:INFO Looking up 1- 
#:DEBUG Applying primitive #<procedure (? n)> 
hey
>> #:DEBUG Analyzing (begin) 
#:DEBUG Fetching syntax rules for begin 
#:DEBUG Analyzing () 
#:INFO Atom () 
()
>> #:DEBUG Analyzing (begin (quote ho)) 
#:DEBUG Fetching syntax rules for begin 
#:DEBUG Analyzing (quote ho) 
#:DEBUG Fetching syntax rules for quote 
ho
>> #:DEBUG Analyzing (define (x=1) (set! x 1) 1) 
#:DEBUG Fetching syntax rules for define 
#:DEBUG Making lambda () -> ((set! x 1) 1) 
#:DEBUG Analyzing (lambda () (set! x 1) 1) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing (set! x 1) 
#:DEBUG Fetching syntax rules for set! 
#:DEBUG Analyzing 1 
#:INFO Atom 1 
#:DEBUG Analyzing 1 
#:INFO Atom 1 
#:INFO Define x=1 : (lambda () (set! x 1) 1) 
ok
>> #:DEBUG Analyzing (define (x=2) (set! x 2) 2) 
#:DEBUG Fetching syntax rules for define 
#:DEBUG Making lambda () -> ((set! x 2) 2) 
#:DEBUG Analyzing (lambda () (set! x 2) 2) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing (set! x 2) 
#:DEBUG Fetching syntax rules for set! 
#:DEBUG Analyzing 2 
#:INFO Atom 2 
#:DEBUG Analyzing 2 
#:INFO Atom 2 
#:INFO Define x=2 : (lambda () (set! x 2) 2) 
ok
>> #:DEBUG Analyzing (define x 0) 
#:DEBUG Fetching syntax rules for define 
#:DEBUG Analyzing 0 
#:INFO Atom 0 
#:INFO Define x : 0 
ok
>> #:DEBUG Analyzing x 
#:INFO Looking up x 
0
>> #:DEBUG Analyzing (+ (x=1) (x=2)) 
#:INFO Analyzing application (+ (x=1) (x=2)) 
#:DEBUG Analyzing + 
#:DEBUG Analyzing (x=1) 
#:INFO Analyzing application (x=1) 
#:DEBUG Analyzing x=1 
#:DEBUG Analyzing (x=2) 
#:INFO Analyzing application (x=2) 
#:DEBUG Analyzing x=2 
#:INFO Executing (+ (x=1) (x=2)) 
#:INFO Looking up + 
#:INFO Executing (x=1) 
#:INFO Looking up x=1 
#:INFO Assign x = 1 
#:INFO Executing (x=2) 
#:INFO Looking up x=2 
#:INFO Assign x = 2 
#:DEBUG Applying primitive #<procedure C_plus> 
3
>> #:DEBUG Analyzing x 
#:INFO Looking up x 
2
>> #:DEBUG Analyzing (set! x 0) 
#:DEBUG Fetching syntax rules for set! 
#:DEBUG Analyzing 0 
#:INFO Atom 0 
#:INFO Assign x = 0 
ok
>> #:DEBUG Analyzing x 
#:INFO Looking up x 
0
>> #:DEBUG Analyzing (+ (x=2) (x=1)) 
#:INFO Analyzing application (+ (x=2) (x=1)) 
#:DEBUG Analyzing + 
#:DEBUG Analyzing (x=2) 
#:INFO Analyzing application (x=2) 
#:DEBUG Analyzing x=2 
#:DEBUG Analyzing (x=1) 
#:INFO Analyzing application (x=1) 
#:DEBUG Analyzing x=1 
#:INFO Executing (+ (x=2) (x=1)) 
#:INFO Looking up + 
#:INFO Executing (x=2) 
#:INFO Looking up x=2 
#:INFO Assign x = 2 
#:INFO Executing (x=1) 
#:INFO Looking up x=1 
#:INFO Assign x = 1 
#:DEBUG Applying primitive #<procedure C_plus> 
3
>> #:DEBUG Analyzing x 
#:INFO Looking up x 
1
>> #:DEBUG Analyzing (set! x 0) 
#:DEBUG Fetching syntax rules for set! 
#:DEBUG Analyzing 0 
#:INFO Atom 0 
#:INFO Assign x = 0 
ok
>> #:DEBUG Analyzing (let ((x 7) (y 8)) (+ x y)) 
#:DEBUG Fetching syntax rules for let 
#:DEBUG Making lambda (x y) -> ((+ x y)) 
#:DEBUG Analyzing ((lambda (x y) (+ x y)) 7 8) 
#:INFO Analyzing application ((lambda (x y) (+ x y)) 7 8) 
#:DEBUG Analyzing (lambda (x y) (+ x y)) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing (+ x y) 
#:INFO Analyzing application (+ x y) 
#:DEBUG Analyzing + 
#:DEBUG Analyzing x 
#:DEBUG Analyzing y 
#:DEBUG Analyzing 7 
#:INFO Atom 7 
#:DEBUG Analyzing 8 
#:INFO Atom 8 
#:INFO Executing ((lambda (x y) (+ x y)) 7 8) 
#:INFO Executing (+ x y) 
#:INFO Looking up + 
#:INFO Looking up x 
#:INFO Looking up y 
#:DEBUG Applying primitive #<procedure C_plus> 
15
>> #:DEBUG Analyzing (let ((x 8) (y 7)) y) 
#:DEBUG Fetching syntax rules for let 
#:DEBUG Making lambda (x y) -> (y) 
#:DEBUG Analyzing ((lambda (x y) y) 8 7) 
#:INFO Analyzing application ((lambda (x y) y) 8 7) 
#:DEBUG Analyzing (lambda (x y) y) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing y 
#:DEBUG Analyzing 8 
#:INFO Atom 8 
#:DEBUG Analyzing 7 
#:INFO Atom 7 
#:INFO Executing ((lambda (x y) y) 8 7) 
#:INFO Looking up y 
7
>> #:DEBUG Analyzing (let ((x 7) (y 8)) (+ x y) y) 
#:DEBUG Fetching syntax rules for let 
#:DEBUG Making lambda (x y) -> ((+ x y) y) 
#:DEBUG Analyzing ((lambda (x y) (+ x y) y) 7 8) 
#:INFO Analyzing application ((lambda (x y) (+ x y) y) 7 8) 
#:DEBUG Analyzing (lambda (x y) (+ x y) y) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing (+ x y) 
#:INFO Analyzing application (+ x y) 
#:DEBUG Analyzing + 
#:DEBUG Analyzing x 
#:DEBUG Analyzing y 
#:DEBUG Analyzing y 
#:DEBUG Analyzing 7 
#:INFO Atom 7 
#:DEBUG Analyzing 8 
#:INFO Atom 8 
#:INFO Executing ((lambda (x y) (+ x y) y) 7 8) 
#:INFO Executing (+ x y) 
#:INFO Looking up + 
#:INFO Looking up x 
#:INFO Looking up y 
#:DEBUG Applying primitive #<procedure C_plus> 
#:INFO Looking up y 
8
>> #:DEBUG Analyzing (define (call* f g) (let* ((b (g)) (a (f))) (+ a b))) 
#:DEBUG Fetching syntax rules for define 
#:DEBUG Making lambda (f g) -> ((let* ((b (g)) (a (f))) (+ a b))) 
#:DEBUG Analyzing (lambda (f g) (let* ((b (g)) (a (f))) (+ a b))) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing (let* ((b (g)) (a (f))) (+ a b)) 
#:DEBUG Fetching syntax rules for let* 
#:DEBUG Wrapping ((+ a b)) 
#:DEBUG Letting ((a (f))) into scope (+ a b) 
#:DEBUG Wrapping (let ((a (f))) (+ a b)) 
#:DEBUG Letting ((b (g))) into scope (begin (let ((a (f))) (+ a b))) 
#:DEBUG Analyzing (let ((b (g))) (begin (let ((a (f))) (+ a b)))) 
#:DEBUG Fetching syntax rules for let 
#:DEBUG Making lambda (b) -> ((begin (let ((a (f))) (+ a b)))) 
#:DEBUG Analyzing ((lambda (b) (begin (let ((a (f))) (+ a b)))) (g)) 
#:INFO Analyzing application ((lambda (b) (begin (let ((a (f))) (+ a b)))) (g)) 
#:DEBUG Analyzing (lambda (b) (begin (let ((a (f))) (+ a b)))) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing (begin (let ((a (f))) (+ a b))) 
#:DEBUG Fetching syntax rules for begin 
#:DEBUG Analyzing (let ((a (f))) (+ a b)) 
#:DEBUG Fetching syntax rules for let 
#:DEBUG Making lambda (a) -> ((+ a b)) 
#:DEBUG Analyzing ((lambda (a) (+ a b)) (f)) 
#:INFO Analyzing application ((lambda (a) (+ a b)) (f)) 
#:DEBUG Analyzing (lambda (a) (+ a b)) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing (+ a b) 
#:INFO Analyzing application (+ a b) 
#:DEBUG Analyzing + 
#:DEBUG Analyzing a 
#:DEBUG Analyzing b 
#:DEBUG Analyzing (f) 
#:INFO Analyzing application (f) 
#:DEBUG Analyzing f 
#:DEBUG Analyzing (g) 
#:INFO Analyzing application (g) 
#:DEBUG Analyzing g 
#:INFO Define call* : (lambda (f g) (let* ((b (g)) (a (f))) (+ a b))) 
ok
>> #:DEBUG Analyzing x 
#:INFO Looking up x 
0
>> #:DEBUG Analyzing (call* x=1 x=2) 
#:INFO Analyzing application (call* x=1 x=2) 
#:DEBUG Analyzing call* 
#:DEBUG Analyzing x=1 
#:DEBUG Analyzing x=2 
#:INFO Executing (call* x=1 x=2) 
#:INFO Looking up call* 
#:INFO Looking up x=1 
#:INFO Looking up x=2 
#:INFO Executing ((lambda (b) (begin (let ((a (f))) (+ a b)))) (g)) 
#:INFO Executing (g) 
#:INFO Looking up g 
#:INFO Assign x = 2 
#:INFO Executing ((lambda (a) (+ a b)) (f)) 
#:INFO Executing (f) 
#:INFO Looking up f 
#:INFO Assign x = 1 
#:INFO Executing (+ a b) 
#:INFO Looking up + 
#:INFO Looking up a 
#:INFO Looking up b 
#:DEBUG Applying primitive #<procedure C_plus> 
3
>> #:DEBUG Analyzing x 
#:INFO Looking up x 
1
>> #:DEBUG Analyzing (set! x 0) 
#:DEBUG Fetching syntax rules for set! 
#:DEBUG Analyzing 0 
#:INFO Atom 0 
#:INFO Assign x = 0 
ok
>> #:DEBUG Analyzing x 
#:INFO Looking up x 
0
>> #:DEBUG Analyzing (call* x=2 x=1) 
#:INFO Analyzing application (call* x=2 x=1) 
#:DEBUG Analyzing call* 
#:DEBUG Analyzing x=2 
#:DEBUG Analyzing x=1 
#:INFO Executing (call* x=2 x=1) 
#:INFO Looking up call* 
#:INFO Looking up x=2 
#:INFO Looking up x=1 
#:INFO Executing ((lambda (b) (begin (let ((a (f))) (+ a b)))) (g)) 
#:INFO Executing (g) 
#:INFO Looking up g 
#:INFO Assign x = 1 
#:INFO Executing ((lambda (a) (+ a b)) (f)) 
#:INFO Executing (f) 
#:INFO Looking up f 
#:INFO Assign x = 2 
#:INFO Executing (+ a b) 
#:INFO Looking up + 
#:INFO Looking up a 
#:INFO Looking up b 
#:DEBUG Applying primitive #<procedure C_plus> 
3
>> #:DEBUG Analyzing x 
#:INFO Looking up x 
2
>> #:DEBUG Analyzing (cond ((assoc (quote b) (quote ((a 1) (b 2)))) => cadr)) 
#:DEBUG Fetching syntax rules for cond 
#:DEBUG Wrapping (cadr (assoc (quote b) (quote ((a 1) (b 2))))) 
#:DEBUG Analyzing (if (assoc (quote b) (quote ((a 1) (b 2)))) (begin (cadr (assoc (quote b) (quote ((a 1) (b 2))))))) 
#:DEBUG Fetching syntax rules for if 
#:DEBUG Analyzing (assoc (quote b) (quote ((a 1) (b 2)))) 
#:INFO Analyzing application (assoc (quote b) (quote ((a 1) (b 2)))) 
#:DEBUG Analyzing assoc 
#:DEBUG Analyzing (quote b) 
#:DEBUG Fetching syntax rules for quote 
#:DEBUG Analyzing (quote ((a 1) (b 2))) 
#:DEBUG Fetching syntax rules for quote 
#:DEBUG Analyzing (begin (cadr (assoc (quote b) (quote ((a 1) (b 2)))))) 
#:DEBUG Fetching syntax rules for begin 
#:DEBUG Analyzing (cadr (assoc (quote b) (quote ((a 1) (b 2))))) 
#:INFO Analyzing application (cadr (assoc (quote b) (quote ((a 1) (b 2))))) 
#:DEBUG Analyzing cadr 
#:DEBUG Analyzing (assoc (quote b) (quote ((a 1) (b 2)))) 
#:INFO Analyzing application (assoc (quote b) (quote ((a 1) (b 2)))) 
#:DEBUG Analyzing assoc 
#:DEBUG Analyzing (quote b) 
#:DEBUG Fetching syntax rules for quote 
#:DEBUG Analyzing (quote ((a 1) (b 2))) 
#:DEBUG Fetching syntax rules for quote 
#:DEBUG Analyzing false 
#:INFO Conditionally (assoc (quote b) (quote ((a 1) (b 2)))) ? (begin (cadr (assoc (quote b) (quote ((a 1) (b 2)))))) : false 
#:INFO Executing (assoc (quote b) (quote ((a 1) (b 2)))) 
#:INFO Looking up assoc 
#:DEBUG Applying primitive #<procedure (scheme#assoc x lst)> 
#:INFO Executing (cadr (assoc (quote b) (quote ((a 1) (b 2))))) 
#:INFO Looking up cadr 
#:INFO Executing (assoc (quote b) (quote ((a 1) (b 2)))) 
#:INFO Looking up assoc 
#:DEBUG Applying primitive #<procedure (scheme#assoc x lst)> 
#:DEBUG Applying primitive #<procedure (scheme#cadr x)> 
2
>> #:DEBUG Analyzing (define (test flag) (cond (else (quote other-option)) (flag (quote should-be-this)))) 
#:DEBUG Fetching syntax rules for define 
#:DEBUG Making lambda (flag) -> ((cond (else (quote other-option)) (flag (quote should-be-this)))) 
#:DEBUG Analyzing (lambda (flag) (cond (else (quote other-option)) (flag (quote should-be-this)))) 
#:DEBUG Fetching syntax rules for lambda 
#:DEBUG Analyzing (cond (else (quote other-option)) (flag (quote should-be-this))) 
#:DEBUG Fetching syntax rules for cond 
WARNING: ELSE clause isn't last -- COND ((else (quote other-option)) (flag (quote should-be-this))) 
#:DEBUG Analyzing #<unspecified> 
ERROR: Unknown expression type -- ANALYZE #<unspecified> 
#:INFO Define test : (lambda (flag) (cond (else (quote other-option)) (flag (quote should-be-this)))) 
ok
>> #:DEBUG Analyzing (cond) 
#:DEBUG Fetching syntax rules for cond 
#:DEBUG Analyzing () 
#:INFO Atom () 
()
>> #:DEBUG Analyzing (cond (else 1)) 
#:DEBUG Fetching syntax rules for cond 
#:DEBUG Wrapping (1) 
#:DEBUG Analyzing 1 
#:INFO Atom 1 
1
>> 
#:INFO Shut-down 
*** bye! ***
