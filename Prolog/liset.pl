append([], Y, Y).
append([U|V], Y, [U|Z]) :- append(V, Y, Z).

length_([], 0).
length_([_|T], L) :- length_(T, Lt), L is Lt + 1.

member(X, [X|_]) :- !.
member(X, [_|T]) :- member(X, T), !.

cons(X, L, [X|L]).

list_nth(0, [H|_], H).
list_nth(N, [_|T], X) :- N1 is N-1, list_nth(N1, T, X).

list_to_set([], []).
list_to_set([H|T], [H|L]):- not(member(H,T)), list_to_set(T,L).
list_to_set([H|T], L):- member(H,T), list_to_set(T,L).

powerset([], []).
powerset([_|S], Sub) :- powerset(S, Sub).
powerset([H|S], [H|Sub]) :- powerset(S, Sub).

intersection([], _, []).
intersection([H|T], S, [H|R]) :- member(H, S), intersection(T, S, R).
intersection([H|T], S, R) :- not(member(H, S)), intersection(T, S, R).

permutation([], []).
permutation([H|T], P) :- permutation(T, Sub), combine(H, Sub, P).

combine(X, [], [X]).
combine(X, [H|T], [X,H|T]).
combine(X, [H|T], [H|Sub]) :- combine(X,T,Sub).

sorted([]).
sorted([_]).
sorted([A,B|T]) :- A =< B, sorted([B|T]).

isort([], []).
isort([H|T], L) :- isort(T,S), insert(H, S, L).

insert(X, [], [X]).
insert(X, [H|T], [X,H|T]) :- X =< H.
insert(X, [H|T], [H|T2]) :- X > H, insert(X,T,T2).
