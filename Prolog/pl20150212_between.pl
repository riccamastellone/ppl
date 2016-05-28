% BetweenAB
% Define a predicate betweenAB, which, given a list L, checks if all the atoms a in L are at the beginning, and
% all the atoms b are at the end of L. E.g. betweenAB([a,a,c,c,d,b,b,b]) is true.

betweenAB([]) :- !.
betweenAB([_]) :- !.
betweenAB([a|Xs]) :- consumeA(Xs).

consumeA([a|Xs]) :- !, consumeA(Xs).
consumeA(X) :- !, consume(X).

consume([b|Xs]) :- !, consumeB(Xs).
consume([X|Xs]) :- !, X \= a, consume(Xs).
consume([]) :- !.

consumeB([b|Xs]) :- !, consumeB(Xs).
consumeB([]) :- !.

% DeepBetweenAB
% Define a “deep” version of betweenAB, which checks that all the lists in L have the same property of having
% all the atoms a at the beginning, and all the atoms b at the end.
% E.g.
% > deepBetweenAB([a,a,[a,[a,b],b],c,[a,a,c,c,[a,b,b,b],b],b,b]).
% true

deepBetweenAB(X) :- betweenAB(X), !, checkAtom(X).

checkAtom([X]) :- !, ( atom(X) ; deepBetweenAB(X) ).
checkAtom([X|Xs]) :- ( atom(X) ; deepBetweenAB(X) ), !, checkAtom(Xs).
