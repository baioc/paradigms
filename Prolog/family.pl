man('Adam').
man('Cain').
man('Enoch').
man('Irad').
man('Mehujael').
man('Methushael').
man('Lamech').
man('Jabal').
man('Jubal').
man(tom).
man(bob).
man(jim).
man(bill).
woman('Ada').
woman(pam).
woman(liz).
woman(pat).
woman(ana).

wife('Lamech', 'Ada').

parent('Adam', 'Cain').
parent('Cain', 'Enoch').
parent('Enoch', 'Irad').
parent('Irad', 'Mehujael').
parent('Mehujael', 'Methushael').
parent('Methushael', 'Lamech').
parent('Ada', 'Jabal').
parent('Ada', 'Jubal').
parent(pam, bob).
parent(tom, bob).
parent(tom, liz).
parent(bob, ana).
parent(bob, pat).
parent(liz, bill).
parent(pat, jim).

parent(M,S) :- wife(M,W), parent(W,S).
mother(X,Y) :- parent(X,Y), woman(X).
father(X,Y) :- parent(X,Y), man(X).

grandparent(G,S) :- parent(F,S), parent(G,F).
grandmother(X,Y) :- grandparent(X,Y), woman(X).
grandfather(X,Y) :- grandparent(X,Y), man(X).

brother(X,Y) :- parent(P,X), parent(P,Y), X \== Y, man(X).
sister(X,Y) :- parent(P,X), parent(P,Y), X \== Y, woman(X).
sibling(X,Y) :- brother(X,Y); sister(X,Y).

ascendant(X,Y) :- parent(X,Y).                 % recursion base case
ascendant(X,Y) :- parent(X,Z), ascendant(Z,Y). % recursive step

descendant(X,Y) :- ascendant(Y,X).
