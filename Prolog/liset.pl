append([], Y, Y).
append([U|V], Y, [U|Z]) :- append(V,Y,Z).

length_([], 0).
length_([_|T], L) :- length_(T,Lt), L is Lt+1.

member(X, [X|_]).
member(X, [_|T]) :- member(X,T), !.

cons(X, L, [X|L]).

list_nth(0, [H|_], H).
list_nth(N, [_|T], X) :- N1 is N-1, list_nth(N1,T,X).

sum([], 0).
sum([H|T], S) :- sum(T,R), S is H + R.

palindrome([]).
palindrome([_]).
palindrome([H|T]) :- last_(H,T), snoc(H,R,T), palindrome(R).

last_(X, [X]).
last_(X, L) :- append(_, [X], L).

snoc(X, [], [X]).
snoc(X, [H|T], [H|R]) :- snoc(X,T,R).

sorted([]).
sorted([_]).
sorted([A,B|T]) :- A =< B, sorted([B|T]).

isort([], []).
isort([H|T], L) :- isort(T,S), insert(H,S,L).

insert(X, [], [X]).
insert(X, [H|T], [X,H|T]) :- X =< H.
insert(X, [H|T], [H|T2]) :- X > H, insert(X,T,T2).


list_to_set([], []).
list_to_set([H|T], [H|L]):- not(member(H,T)), list_to_set(T,L).
list_to_set([H|T], L):- member(H,T), list_to_set(T,L).

union([], S, S).
union([H|T], S, [H|R]) :- not(member(H,S)), union(T,S,R).
union([H|T], S, R) :- member(H,S), union(T,S,R).

intersection([], _, []).
intersection([H|T], S, [H|R]) :- member(H,S), intersection(T,S,R).
intersection([H|T], S, R) :- not(member(H,S)), intersection(T,S,R).

difference([], _, []).
difference([H|T], B, C) :- member(H,B), difference(T,B,C).
difference([H|T], B, [H|C]) :- not(member(H,B)), difference(T,B,C).

powerset([], []).
powerset([_|T], S) :- powerset(T, S).
powerset([H|T], [H|S]) :- powerset(T, S).

permutation([], []).
permutation([H|T], P) :- permutation(T, S), combine(H, S, P).

combine(X, [], [X]).
combine(X, [H|T], [X,H|T]).
combine(X, [H|T], [H|S]) :- combine(X, T, S).
