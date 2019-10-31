fatorial(1,1).
fatorial(N,K) :- N1 is N - 1, fatorial(N1, K1), K is N * K1.

fibonacci(0,0).
fibonacci(1,1).
fibonacci(N,Fn) :- N2 is N-2, fibonacci(N2, Fn2),
                   N1 is N-1, fibonacci(N1, Fn1),
                   Fn is Fn1 + Fn2.

:- initialization(main).
main :-
    write('N: '),
    read(N),

    write('N! = '),
    fatorial(N, Fact_N),
    writeln(Fact_N),

    write('fib(N) = '),
    fibonacci(N, Fib_N),
    writeln(Fib_N),

    halt.
