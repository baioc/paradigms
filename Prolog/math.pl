factorial(1,1).
factorial(N,K) :- N1 is N - 1, factorial(N1, K1), K is N * K1, !.

fibonacci(0,0).
fibonacci(1,1).
fibonacci(N,Fn) :- N2 is N-2, fibonacci(N2, Fn2),
                   N1 is N-1, fibonacci(N1, Fn1),
                   Fn is Fn1 + Fn2, !.

divisible(A,B) :- 0 is A mod B.

triangle(A,B,C) :- A < B + C, B < A + C, C < A + B.

prime(2).
prime(3).
prime(N) :- N > 3, not(divisible(N,2)), prime_aux_(N,3).
prime_aux_(N, K) :- (K*K > N, !);
                    (not(divisible(N,K)), K2 is K+2, prime_aux_(N,K2), !).

:- initialization(main).
main :-
    write('N: '),
    read(N),

    write('N! = '),
    factorial(N, Fact_N),
    writeln(Fact_N),

    write('fib(N) = '),
    fibonacci(N, Fib_N),
    writeln(Fib_N),

    halt.
