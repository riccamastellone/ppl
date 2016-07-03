% Consider a variant of the problem seen in Exercise 1: an unsorted list can contain either atoms or integer
% numbers. E.g. [1,3,house,4,of,-7,cards,end,-12].
% Define a demux predicate, that has an unsorted list as first argument, the sum n of the numbers present
% in the unsorted list as second argument, and puts all the symbols in the list v as third argument. demux must
% stop if an atom end is found, and must be optimized with cut.
% E.g. for the previous example list n = 1 and v = [house,of,cards].

demux([end|_],0,[]) :- !.
demux([X|Xs],N,V) :- number(X), !, demux(Xs,N1,V), !, N is N1 + X.
demux([X|Xs],N,[X|V]) :- !, demux(Xs,N,V).
demux([],0,[]) :- !.

% Define a shuffle predicate, that takes three lists n, a, m, where n is a list of number, a is a list of atoms,
% and m is a list of numbers and atoms containing the elements of n and a, by maintaining their relative order
% (e.g. shuffle([1,2],[house,next,foot],[house,1,next,foot,2]) must hold). shuffle must be able to
% shuffle two lists together, e.g. to obtain as output the third list in the example.

shuffle([],[],[]).
shuffle(V,[X|Ys],[X|Xs]) :- atom(X), shuffle(V,Ys,Xs).
shuffle([X|Ys],V,[X|Xs]) :- number(X), shuffle(Ys,V,Xs).
