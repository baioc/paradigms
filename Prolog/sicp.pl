job('Bitdiddle Ben', ['Computer', 'Wizard']).
job('Hacker Alyssa P', ['Computer', 'Programmer']).
job('Fect Cy D', ['Computer', 'Programmer']).
job('Tweakit Lem E', ['Computer', 'Technician']).
job('Reasoner Louis', ['Computer', 'Programmer', 'Trainee']).
job('Warbucks Oliver', ['Administration', 'Big Wheel']).
job('Scrooge Eben', ['Accounting', 'Chief Accountant']).
job('Cratchet Robert', ['Accounting', 'Scrivener']).
job('Aull DeWitt', ['Administration', 'Secretary']).

salary('Bitdiddle Ben', 60000).
salary('Hacker Alyssa P', 40000).
salary('Fect Cy D', 35000).
salary('Tweakit Lem E', 25000).
salary('Reasoner Louis', 30000).
salary('Warbucks Oliver', 150000).
salary('Scrooge Eben', 75000).
salary('Aull DeWitt', 25000).
salary('Cratchet Robert', 18000).

supervisor('Bitdiddle Ben', 'Warbucks Oliver').
supervisor('Hacker Alyssa P', 'Bitdiddle Ben').
supervisor('Fect Cy D', 'Bitdiddle Ben').
supervisor('Tweakit Lem E', 'Bitdiddle Ben').
supervisor('Reasoner Louis', 'Hacker Alyssa P').
supervisor('Scrooge Eben', 'Warbucks Oliver').
supervisor('Cratchet Robert', 'Scrooge Eben').
supervisor('Aull DeWitt', 'Warbucks Oliver').

address('Bitdiddle Ben', ['Slumerville', 'Ridge Road', 10]).
address('Hacker Alyssa P', ['Cambridge', 'Mass Ave', 78]).
address('Tweakit Lem E', ['Boston', 'Bay State Road', 22]).
address('Fect Cy D', ['Cambridge', 'Ames Street', 3]).
address('Reasoner Louis', ['Slumerville', 'Pine Tree Road', 80]).
address('Warbucks Oliver', ['Swellesley', 'Top Heap Road']).
address('Scrooge Eben', ['Weston', 'Shady Lane', 10]).
address('Cratchet Robert', ['Allston', 'N Harvard Street', 16]).
address('Aull DeWitt', ['Slumerville', 'Onion Square', 5]).

can_do_job(['Computer', 'Wizard'], ['Computer', 'Programmer']).
can_do_job(['Computer', 'Wizard'], ['Computer', 'Technician']).
can_do_job(['Computer', 'Programmer'], ['Computer', 'Programmer', 'Trainee']).
can_do_job(['Administration', 'Secretary'], ['Administration', 'Big Wheel']).

lives_near(X,Y) :- address(X,[Town|_]), address(Y,[Town|_]), X \= Y.
wheel(X) :- supervisor(Y,X), supervisor(_,Y).
can_replace(X,Y) :- job(X,Jx), job(Y,Jy), X \= Y,
                    (Jx = Jy; can_do_job(Jx,Jy)).

query :-
    forall(
        (supervisor(Who, 'Bitdiddle Ben'),
         address(Who, Where)),
        (write((Who, Where)), nl)),
    nl,

    forall(
        (job(Who, ['Accounting'|Job]),
         address(Who, Where)),
        (write((Job, Where)), nl)),
    nl,

    forall(
        address(Who, ['Slumerville'|Where]),
        (write((Who, ['Slumerville'|Where])), nl)),
    nl,

    forall(
        (salary('Bitdiddle Ben', Ref),
         salary(Who, Amount),
         Amount < Ref),
        (write((Who, Amount, Ref)), nl)),
    nl,

    forall(
        (job(Superv, [Division|Job]),
         Division \= 'Computer',
         supervisor(Person, Superv)),
        (write((Person, Superv, Job)), nl)),
    nl,

    forall(
        (can_replace(Who, Someone),
         salary(Who, Sw),
         salary(Someone, Ss),
         Ss > Sw),
        (write((Who, Sw, Someone, Ss)), nl)),
    nl,

    true.

:- initialization(query).
