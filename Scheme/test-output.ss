INFO: Logger verbosity at level DEBUG 
INFO: Setting-up environment... (false true + - = < > not <= >= * / zero? negative? positive? quotient remainder modulo equal? eqv? eq? symbol? pair? boolean? number? inexact? exact? inexact->exact exact->inexact complex? real? rational? integer? nan? exit error read write port? eof-object? display newline input-port? open-input-file close-input-port output-port? open-output-file close-output-port read-char write-char peek-char string? string-length string-ref string-set! string=? string-ci=? string<? string-ci<? string<=? string-ci<=? string>? string-ci>? string>=? string-ci>=? substring string-append string-copy string->number number->string symbol->string string->symbol char? char-upcase char-downcase char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case? char=? char-ci=? char<? char-ci<? char<=? char-ci<=? char>? char-ci>? char>=? char-ci>=? integer->char char->integer char-alphanumeric? cons car cdr set-car! set-cdr! list list? null? length append reverse list-tail list-ref assq assv assoc memq memv member string->list list->string floor ceiling truncate round square abs gcd lcm expt sqrt odd? even? max min exp log sin cos tan asin acos atan caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr cddar cdddr caaaar caaadr caadar cadaar cdaaar caaddr cadadr caddar cdaadr cdadar cddaar cadddr cdaddr cddadr cdddar cddddr) 
INFO: Greetings! 
  ___ ____________  ___  _____  ___ ______  ___ ____________  ___
 / _ \| ___ | ___ \/ _ \/  __ \/ _ \|  _  \/ _ \| ___ | ___ \/ _ \
/ /_\ | |_/ | |_/ / /_\ | /  \/ /_\ | | | / /_\ | |_/ | |_/ / /_\ \
|  _  | ___ |    /|  _  | |   |  _  | | | |  _  | ___ |    /|  _  |
| | | | |_/ | |\ \| | | | \__/| | | | |/ /| | | | |_/ | |\ \| | | |
\_| |_\____/\_| \_\_| |_/\____\_| |_|___/ \_| |_\____/\_| \_\_| |_/
(c) 2019, Gabriel B. Sant'Anna
Version 0.3.bb
>> DEBUG: Analyzing 42 
INFO: Atom 42 
42
>> DEBUG: Analyzing 0.03 
INFO: Atom 0.03 
0.03
>> DEBUG: Analyzing 0.0012 
INFO: Atom 0.0012 
0.0012
>> DEBUG: Analyzing 1e-90 
INFO: Atom 1e-90 
1e-90
>> DEBUG: Analyzing 3/5 
INFO: Atom 3/5 
3/5
>> DEBUG: Analyzing 3+4i 
INFO: Atom 3+4i 
3+4i
>> DEBUG: Analyzing #\c 
INFO: Atom #\c 
#\c
>> DEBUG: Analyzing "this is a string" 
INFO: Atom "this is a string" 
"this is a string"
>> DEBUG: Analyzing (quote a) 
DEBUG: Fetching syntax rules quote 
a
>> DEBUG: Analyzing (quote (quote a)) 
DEBUG: Fetching syntax rules quote 
(quote a)
>> DEBUG: Analyzing (quote (a b c)) 
DEBUG: Fetching syntax rules quote 
(a b c)
>> DEBUG: Analyzing #t 
INFO: Looking up #t 
WARNING: Unbound variable #t 
>> DEBUG: Analyzing true 
INFO: Looking up true 
true
>> DEBUG: Analyzing #f 
INFO: Looking up #f 
WARNING: Unbound variable #f 
>> DEBUG: Analyzing false 
INFO: Looking up false 
false
>> DEBUG: Analyzing (set! k -1) 
DEBUG: Fetching syntax rules set! 
DEBUG: Analyzing -1 
INFO: Atom -1 
WARNING: Unbound variable! k 
>> DEBUG: Analyzing k 
INFO: Looking up k 
WARNING: Unbound variable k 
>> DEBUG: Analyzing (define k 99) 
DEBUG: Fetching syntax rules define 
DEBUG: Analyzing 99 
INFO: Atom 99 
>> DEBUG: Analyzing k 
INFO: Looking up k 
99
>> DEBUG: Analyzing (set! k -88) 
DEBUG: Fetching syntax rules set! 
DEBUG: Analyzing -88 
INFO: Atom -88 
>> DEBUG: Analyzing k 
INFO: Looking up k 
-88
>> DEBUG: Analyzing eq? 
INFO: Looking up eq? 
#<procedure eq?>
>> DEBUG: Analyzing (eq? false (quote false)) 
DEBUG: Analyzing false 
DEBUG: Analyzing (quote false) 
DEBUG: Fetching syntax rules quote 
DEBUG: Analyzing eq? 
INFO: Looking up false 
INFO: Looking up eq? 
INFO: Applying primitive #<procedure eq?> to (false false) 
true
>> DEBUG: Analyzing (eq? (quote lambda) (quote lambda)) 
DEBUG: Analyzing (quote lambda) 
DEBUG: Fetching syntax rules quote 
DEBUG: Analyzing (quote lambda) 
DEBUG: Fetching syntax rules quote 
DEBUG: Analyzing eq? 
INFO: Looking up eq? 
INFO: Applying primitive #<procedure eq?> to (λ λ) 
true
>> DEBUG: Analyzing (+ 1 2 3 4) 
DEBUG: Analyzing 3 
INFO: Atom 3 
DEBUG: Analyzing 4 
INFO: Atom 4 
DEBUG: Analyzing 1 
INFO: Atom 1 
DEBUG: Analyzing 2 
INFO: Atom 2 
DEBUG: Analyzing + 
INFO: Looking up + 
INFO: Applying primitive #<procedure +> to (1 2 3 4) 
10
>> DEBUG: Analyzing (define (test b) (if b (display "#TRUE\n") (display "#FALSE\n"))) 
DEBUG: Fetching syntax rules define 
DEBUG: Making lambda (b) -> ((if b (display "#TRUE\n") (display "#FALSE\n"))) 
DEBUG: Analyzing (lambda (b) (if b (display "#TRUE\n") (display "#FALSE\n"))) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (if b (display "#TRUE\n") (display "#FALSE\n")) 
DEBUG: Fetching syntax rules if 
DEBUG: Analyzing (display "#FALSE\n") 
DEBUG: Analyzing "#FALSE\n" 
INFO: Atom "#FALSE\n" 
DEBUG: Analyzing display 
DEBUG: Analyzing (display "#TRUE\n") 
DEBUG: Analyzing "#TRUE\n" 
INFO: Atom "#TRUE\n" 
DEBUG: Analyzing display 
DEBUG: Analyzing b 
>> DEBUG: Analyzing (test (quote #f)) 
DEBUG: Analyzing (quote #f) 
DEBUG: Fetching syntax rules quote 
DEBUG: Analyzing test 
INFO: Looking up test 
INFO: Looking up b 
INFO: Looking up display 
INFO: Applying primitive #<procedure display> to ("#TRUE\n") 
#TRUE
#<void>
>> DEBUG: Analyzing (test false) 
DEBUG: Analyzing false 
DEBUG: Analyzing test 
INFO: Looking up false 
INFO: Looking up test 
INFO: Looking up b 
INFO: Looking up display 
INFO: Applying primitive #<procedure display> to ("#FALSE\n") 
#FALSE
#<void>
>> DEBUG: Analyzing (define l1 (lambda (b) b)) 
DEBUG: Fetching syntax rules define 
DEBUG: Analyzing (lambda (b) b) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing b 
>> DEBUG: Analyzing (define (l2 b) b) 
DEBUG: Fetching syntax rules define 
DEBUG: Making lambda (b) -> (b) 
DEBUG: Analyzing (lambda (b) b) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing b 
>> DEBUG: Analyzing (define l3 (lambda (b) (b))) 
DEBUG: Fetching syntax rules define 
DEBUG: Analyzing (lambda (b) (b)) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (b) 
DEBUG: Analyzing b 
>> DEBUG: Analyzing (define (l4) (l3 (l2 newline)) (l3 (l1 newline)) (square 4)) 
DEBUG: Fetching syntax rules define 
DEBUG: Making lambda () -> ((l3 (l2 newline)) (l3 (l1 newline)) (square 4)) 
DEBUG: Analyzing (lambda () (l3 (l2 newline)) (l3 (l1 newline)) (square 4)) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (square 4) 
DEBUG: Analyzing 4 
INFO: Atom 4 
DEBUG: Analyzing square 
DEBUG: Analyzing (l3 (l2 newline)) 
DEBUG: Analyzing (l2 newline) 
DEBUG: Analyzing newline 
DEBUG: Analyzing l2 
DEBUG: Analyzing l3 
DEBUG: Analyzing (l3 (l1 newline)) 
DEBUG: Analyzing (l1 newline) 
DEBUG: Analyzing newline 
DEBUG: Analyzing l1 
DEBUG: Analyzing l3 
>> DEBUG: Analyzing (begin (quote hey) (l4)) 
DEBUG: Fetching syntax rules begin 
DEBUG: Analyzing (quote hey) 
DEBUG: Fetching syntax rules quote 
DEBUG: Analyzing (l4) 
DEBUG: Analyzing l4 
INFO: Looking up l4 
INFO: Looking up newline 
INFO: Looking up l2 
INFO: Looking up b 
INFO: Looking up l3 
INFO: Looking up b 
INFO: Applying primitive #<procedure newline> to () 

INFO: Looking up newline 
INFO: Looking up l1 
INFO: Looking up b 
INFO: Looking up l3 
INFO: Looking up b 
INFO: Applying primitive #<procedure newline> to () 

INFO: Looking up square 
INFO: Applying primitive #<procedure> to (4) 
16
>> DEBUG: Analyzing (begin "ho") 
DEBUG: Fetching syntax rules begin 
DEBUG: Analyzing "ho" 
INFO: Atom "ho" 
"ho"
>> DEBUG: Analyzing (define (x=1) (set! x 1) 1) 
DEBUG: Fetching syntax rules define 
DEBUG: Making lambda () -> ((set! x 1) 1) 
DEBUG: Analyzing (lambda () (set! x 1) 1) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (set! x 1) 
DEBUG: Fetching syntax rules set! 
DEBUG: Analyzing 1 
INFO: Atom 1 
DEBUG: Analyzing 1 
INFO: Atom 1 
>> DEBUG: Analyzing (define (x=2) (set! x 2) 2) 
DEBUG: Fetching syntax rules define 
DEBUG: Making lambda () -> ((set! x 2) 2) 
DEBUG: Analyzing (lambda () (set! x 2) 2) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (set! x 2) 
DEBUG: Fetching syntax rules set! 
DEBUG: Analyzing 2 
INFO: Atom 2 
DEBUG: Analyzing 2 
INFO: Atom 2 
>> DEBUG: Analyzing (define x 0) 
DEBUG: Fetching syntax rules define 
DEBUG: Analyzing 0 
INFO: Atom 0 
>> DEBUG: Analyzing x 
INFO: Looking up x 
0
>> DEBUG: Analyzing (+ (x=1) (x=2)) 
DEBUG: Analyzing (x=1) 
DEBUG: Analyzing x=1 
DEBUG: Analyzing (x=2) 
DEBUG: Analyzing x=2 
DEBUG: Analyzing + 
INFO: Looking up x=2 
INFO: Looking up x=1 
INFO: Looking up + 
INFO: Applying primitive #<procedure +> to (1 2) 
3
>> DEBUG: Analyzing x 
INFO: Looking up x 
1
>> DEBUG: Analyzing (set! x 0) 
DEBUG: Fetching syntax rules set! 
DEBUG: Analyzing 0 
INFO: Atom 0 
>> DEBUG: Analyzing x 
INFO: Looking up x 
0
>> DEBUG: Analyzing (+ (x=2) (x=1)) 
DEBUG: Analyzing (x=2) 
DEBUG: Analyzing x=2 
DEBUG: Analyzing (x=1) 
DEBUG: Analyzing x=1 
DEBUG: Analyzing + 
INFO: Looking up x=1 
INFO: Looking up x=2 
INFO: Looking up + 
INFO: Applying primitive #<procedure +> to (2 1) 
3
>> DEBUG: Analyzing x 
INFO: Looking up x 
2
>> DEBUG: Analyzing (let ((x 7) (y 8)) (+ x y)) 
DEBUG: Fetching syntax rules let 
DEBUG: Making lambda (x y) -> ((+ x y)) 
DEBUG: Analyzing ((lambda (x y) (+ x y)) 7 8) 
DEBUG: Analyzing 7 
INFO: Atom 7 
DEBUG: Analyzing 8 
INFO: Atom 8 
DEBUG: Analyzing (lambda (x y) (+ x y)) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (+ x y) 
DEBUG: Analyzing x 
DEBUG: Analyzing y 
DEBUG: Analyzing + 
INFO: Looking up y 
INFO: Looking up x 
INFO: Looking up + 
INFO: Applying primitive #<procedure +> to (7 8) 
15
>> DEBUG: Analyzing (let ((x 8) (y 7)) y) 
DEBUG: Fetching syntax rules let 
DEBUG: Making lambda (x y) -> (y) 
DEBUG: Analyzing ((lambda (x y) y) 8 7) 
DEBUG: Analyzing 8 
INFO: Atom 8 
DEBUG: Analyzing 7 
INFO: Atom 7 
DEBUG: Analyzing (lambda (x y) y) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing y 
INFO: Looking up y 
7
>> DEBUG: Analyzing (let ((x 7) (y 8)) (+ x y) y) 
DEBUG: Fetching syntax rules let 
DEBUG: Making lambda (x y) -> ((+ x y) y) 
DEBUG: Analyzing ((lambda (x y) (+ x y) y) 7 8) 
DEBUG: Analyzing 7 
INFO: Atom 7 
DEBUG: Analyzing 8 
INFO: Atom 8 
DEBUG: Analyzing (lambda (x y) (+ x y) y) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (+ x y) 
DEBUG: Analyzing x 
DEBUG: Analyzing y 
DEBUG: Analyzing + 
DEBUG: Analyzing y 
INFO: Looking up y 
INFO: Looking up x 
INFO: Looking up + 
INFO: Applying primitive #<procedure +> to (7 8) 
INFO: Looking up y 
8
>> DEBUG: Analyzing (define (call1* f g) (let* ((b (g)) (a (f))) a)) 
DEBUG: Fetching syntax rules define 
DEBUG: Making lambda (f g) -> ((let* ((b (g)) (a (f))) a)) 
DEBUG: Analyzing (lambda (f g) (let* ((b (g)) (a (f))) a)) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (let* ((b (g)) (a (f))) a) 
DEBUG: Fetching syntax rules let* 
DEBUG: Letting ((a (f))) into scope of a 
DEBUG: Letting ((b (g))) into scope of (let ((a (f))) a) 
DEBUG: Analyzing (let ((b (g))) (let ((a (f))) a)) 
DEBUG: Fetching syntax rules let 
DEBUG: Making lambda (b) -> ((let ((a (f))) a)) 
DEBUG: Analyzing ((lambda (b) (let ((a (f))) a)) (g)) 
DEBUG: Analyzing (g) 
DEBUG: Analyzing g 
DEBUG: Analyzing (lambda (b) (let ((a (f))) a)) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (let ((a (f))) a) 
DEBUG: Fetching syntax rules let 
DEBUG: Making lambda (a) -> (a) 
DEBUG: Analyzing ((lambda (a) a) (f)) 
DEBUG: Analyzing (f) 
DEBUG: Analyzing f 
DEBUG: Analyzing (lambda (a) a) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing a 
>> DEBUG: Analyzing (call1* x=1 x=2) 
DEBUG: Analyzing x=1 
DEBUG: Analyzing x=2 
DEBUG: Analyzing call1* 
INFO: Looking up x=2 
INFO: Looking up x=1 
INFO: Looking up call1* 
INFO: Looking up g 
INFO: Looking up f 
INFO: Looking up a 
1
>> DEBUG: Analyzing (define (call2* f g) (let* ((b (g)) (a (f))) (+ a b))) 
DEBUG: Fetching syntax rules define 
DEBUG: Making lambda (f g) -> ((let* ((b (g)) (a (f))) (+ a b))) 
DEBUG: Analyzing (lambda (f g) (let* ((b (g)) (a (f))) (+ a b))) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (let* ((b (g)) (a (f))) (+ a b)) 
DEBUG: Fetching syntax rules let* 
DEBUG: Letting ((a (f))) into scope of (+ a b) 
DEBUG: Letting ((b (g))) into scope of (let ((a (f))) (+ a b)) 
DEBUG: Analyzing (let ((b (g))) (let ((a (f))) (+ a b))) 
DEBUG: Fetching syntax rules let 
DEBUG: Making lambda (b) -> ((let ((a (f))) (+ a b))) 
DEBUG: Analyzing ((lambda (b) (let ((a (f))) (+ a b))) (g)) 
DEBUG: Analyzing (g) 
DEBUG: Analyzing g 
DEBUG: Analyzing (lambda (b) (let ((a (f))) (+ a b))) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (let ((a (f))) (+ a b)) 
DEBUG: Fetching syntax rules let 
DEBUG: Making lambda (a) -> ((+ a b)) 
DEBUG: Analyzing ((lambda (a) (+ a b)) (f)) 
DEBUG: Analyzing (f) 
DEBUG: Analyzing f 
DEBUG: Analyzing (lambda (a) (+ a b)) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (+ a b) 
DEBUG: Analyzing a 
DEBUG: Analyzing b 
DEBUG: Analyzing + 
>> DEBUG: Analyzing (set! x 0) 
DEBUG: Fetching syntax rules set! 
DEBUG: Analyzing 0 
INFO: Atom 0 
>> DEBUG: Analyzing x 
INFO: Looking up x 
0
>> DEBUG: Analyzing (call2* x=1 x=2) 
DEBUG: Analyzing x=1 
DEBUG: Analyzing x=2 
DEBUG: Analyzing call2* 
INFO: Looking up x=2 
INFO: Looking up x=1 
INFO: Looking up call2* 
INFO: Looking up g 
INFO: Looking up f 
INFO: Looking up b 
INFO: Looking up a 
INFO: Looking up + 
INFO: Applying primitive #<procedure +> to (1 2) 
3
>> DEBUG: Analyzing x 
INFO: Looking up x 
1
>> DEBUG: Analyzing (set! x 0) 
DEBUG: Fetching syntax rules set! 
DEBUG: Analyzing 0 
INFO: Atom 0 
>> DEBUG: Analyzing x 
INFO: Looking up x 
0
>> DEBUG: Analyzing (call2* x=2 x=1) 
DEBUG: Analyzing x=2 
DEBUG: Analyzing x=1 
DEBUG: Analyzing call2* 
INFO: Looking up x=1 
INFO: Looking up x=2 
INFO: Looking up call2* 
INFO: Looking up g 
INFO: Looking up f 
INFO: Looking up b 
INFO: Looking up a 
INFO: Looking up + 
INFO: Applying primitive #<procedure +> to (2 1) 
3
>> DEBUG: Analyzing x 
INFO: Looking up x 
2
>> DEBUG: Analyzing (cond ((assoc (quote b) (quote ((a 1) (b 2)))) => cadr)) 
DEBUG: Fetching syntax rules cond 
DEBUG: Letting ((*temp* (assoc (quote b) (quote ((a 1) (b 2)))))) into scope of (if *temp* (cadr *temp*) false) 
DEBUG: Analyzing (let ((*temp* (assoc (quote b) (quote ((a 1) (b 2)))))) (if *temp* (cadr *temp*) false)) 
DEBUG: Fetching syntax rules let 
DEBUG: Making lambda (*temp*) -> ((if *temp* (cadr *temp*) false)) 
DEBUG: Analyzing ((lambda (*temp*) (if *temp* (cadr *temp*) false)) (assoc (quote b) (quote ((a 1) (b 2))))) 
DEBUG: Analyzing (assoc (quote b) (quote ((a 1) (b 2)))) 
DEBUG: Analyzing (quote b) 
DEBUG: Fetching syntax rules quote 
DEBUG: Analyzing (quote ((a 1) (b 2))) 
DEBUG: Fetching syntax rules quote 
DEBUG: Analyzing assoc 
DEBUG: Analyzing (lambda (*temp*) (if *temp* (cadr *temp*) false)) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (if *temp* (cadr *temp*) false) 
DEBUG: Fetching syntax rules if 
DEBUG: Analyzing false 
DEBUG: Analyzing (cadr *temp*) 
DEBUG: Analyzing *temp* 
DEBUG: Analyzing cadr 
DEBUG: Analyzing *temp* 
INFO: Looking up assoc 
INFO: Applying primitive #<procedure assoc> to (b ((a 1) (b 2))) 
INFO: Looking up *temp* 
INFO: Looking up *temp* 
INFO: Looking up cadr 
INFO: Applying primitive #<procedure cadr> to ((b 2)) 
2
>> DEBUG: Analyzing (define (test flag) (cond (else (quote other-option)) (flag (quote should-be-this)))) 
DEBUG: Fetching syntax rules define 
DEBUG: Making lambda (flag) -> ((cond (else (quote other-option)) (flag (quote should-be-this)))) 
DEBUG: Analyzing (lambda (flag) (cond (else (quote other-option)) (flag (quote should-be-this)))) 
DEBUG: Fetching syntax rules lambda 
DEBUG: Analyzing (cond (else (quote other-option)) (flag (quote should-be-this))) 
DEBUG: Fetching syntax rules cond 
WARNING: ELSE clause isn't last (cond (else 'other-option) (flag 'should-be-this)) 
DEBUG: Analyzing (begin (quote other-option)) 
DEBUG: Fetching syntax rules begin 
DEBUG: Analyzing (quote other-option) 
DEBUG: Fetching syntax rules quote 
>> DEBUG: Analyzing (test true) 
DEBUG: Analyzing true 
DEBUG: Analyzing test 
INFO: Looking up true 
INFO: Looking up test 
other-option
>> DEBUG: Analyzing (cond) 
DEBUG: Fetching syntax rules cond 
DEBUG: Analyzing false 
INFO: Looking up false 
false
>> DEBUG: Analyzing (cond (else 1)) 
DEBUG: Fetching syntax rules cond 
DEBUG: Analyzing (begin 1) 
DEBUG: Fetching syntax rules begin 
DEBUG: Analyzing 1 
INFO: Atom 1 
1
>> 
INFO: Shut-down... 
*** bye! ***
