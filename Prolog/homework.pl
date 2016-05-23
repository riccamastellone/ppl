max([X],X) :- !.
max([X|Xs],X) :- max(Xs,Y), X >= Y.
max([X|Xs],Z) :- max(Xs,Z), Z > X.

min([X],X) :- !.
min([X|Xs],X) :- min(Xs,Y), X =< Y.
min([X|Xs],Z) :- min(Xs,Z), Z < X.

% Funziona, ma è inutile
isMissing([X],Z) :- X \= Z.
isMissing([X|Y],Z) :- X \= Z, isMissing(Y,Z).

% Non si può chiamare flattern/2 poichè esiste già in swi-prolog
flatten2([], []) :- !.
flatten2([X|Xs], FlatL) :- !, flatten2(X, LF), flatten2(Xs, LsF), append(LF, LsF, FlatL).
flatten2(X, FlatL) :- callable(X), X =.. [_|FlatL2], flatten2(FlatL2, FlatL).
flatten2(L, [L]).

find([X|_], X).
find([_|Y], X) :- find(Y, X).

% Output dei valori mancanti non in lista ma come elementi singoli (come richiesto dall'homework)
missing(X,Y) :- missing2(X,L), find(L,Y).

% Ritorniamo la lista di valori mancanti (metodo1)
missing2(Z,Y) :- flatten2(Z, X), min(X, Min), max(X, Max), findall(Num, between(Min, Max, Num), L), subtract(L, X, Y).

% Ritorniamo la lista di valori mancani (metodo2)
missing3([],[]) :- !.
missing3(L,Y) :- flatten2(L, FList), sort(FList, [X|Xs]), missing([X|Xs], X, Y).
missing3([], _, []).
missing3([X|Xs], Curr, [Curr|Y]) :- X =\= Curr, !, Next is Curr + 1, missing([X|Xs], Next, Y).
missing3([_|Xs], Curr, Y) :- Next is Curr + 1, missing(Xs, Next, Y).

missing4(Z,Y) :- flatten2(Z, X), min(X, Min), max(X, Max), Y >= Min, Y < Max, isMissing(Z,Y).