% Define a predicate lile, which, given a list, check if it contains its own length. E.g. lile([2,1]) is true
% while lile([1,2,1]) is not.

lile(X) :- length(X,L), find(X,L).

find([X|_],X) :- !.
find([_|Xs],L) :- find(Xs,L).

% Define a version of the predicate lile, called lileg, which takes a list of any depth, and check if all the lists
% in it contain their length. E.g. lileg([2,[2,[1]],3]) is true.

lileg(X) :-  lile(X), !, sublist(X).

sublist([]) :- !.
sublist([X|Xs]) :- atomic(X), !, sublist(Xs).
sublist([X|Xs]) :- lileg(X), !, sublist(Xs).
