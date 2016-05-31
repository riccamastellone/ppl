% Define the predicate prod_list that returns the product of all the element in a list, optimizing it with cut,
% and without using any library functions.

prod_list([L],L) :- !.
prod_list([L|L1s],Y) :- !, prod_list(L1s,Y1), Y is Y1 * L.

% Define the predicate freeM(X, A) which succeeds if, and only if, X is an element of the free monoid on A,
% i.e. X is a list made of elements taken from A.
% Note: it must be possible to use such predicate also to obtain all the possible lists made of elements of
% the given list A (e.g. if called as freeM(X, [0,1])).

freeM([], _).
freeM([X|Xs], A) :- freeM(Xs, A), member(X, A).
