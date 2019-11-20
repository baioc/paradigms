:- dynamic coins/3.
:- retractall(coins(_,_,_)).
:- dynamic coin/1.
:- retractall(coin(_)).

%% @FIXME: only good for single swipl runs: must retractall coin/1 and coins/3 before new instance
change(Total, Available, Count, Coins) :-
    register(Available),
    coins(Total, Count, Coins).

register([]).
register([First|Rest]) :- asserta(coin(First)), register(Rest).

coins(Sum, 1, [Sum]) :- coin(Sum), !.
coins(Sum, Count, Choices) :-
    Sum > 0,
    findall(
        (K, [X|Coins]),
        (coin(X), X < Sum, Rem is Sum - X, coins(Rem, K, Coins)),
        Possibilities
    ),
    optimal(Possibilities, (MinCount, Choices)),
    Count is MinCount + 1,
    asserta(coins(Sum, Count, Choices) :- !).

optimal([L|Ls], Min) :- optimal(Ls, L, Min).
optimal([], Min, Min).
optimal([(Kh,Ch)|Rest], (Ko,Co), Min) :-
    best((Kh,Ch), (Ko,Co), (Km,Cm)),
    optimal(Rest, (Km,Cm), Min).

best((Ka,Ca), (Kb,_), (Ka,Ca)) :- Ka =< Kb, !.
best((Ka,_), (Kb,Cb), (Kb,Cb)) :- Kb =< Ka, !.


% ?- change(110, [20,30,60], N, L).
% N = 3,
% L = [20, 30, 60].

% ?- change(11, [1,5,6,8], N, L).
% N = 2,
% L = [5, 6].

% ?- change(1000, [35,150], N, L).
% N = 22,
% L = [35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 150, 150]

% ?- change(2000, [35,150], N, L).
% N = 21,
% L = [35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150].

% ?- change(2100, [35,150], N, L).
% N = 14,
% L = [150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150].
