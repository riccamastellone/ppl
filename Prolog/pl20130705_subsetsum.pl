% Define a Prolog predicate subsetsum that checks if there exists a sublist of the first argument such that the
% sum of all its elements is equal to the second argument.
% E.g.: subsetsum([1,2,3,4],19) is false, while subsetsum([1,2,3,4],7) is true
%
subsetsum(L,V) :- process(L,V,0).

process(_,V,V) :- !.
process([L|Ls],V,Acc) :-  process(Ls, V, 0) ; Newacc is Acc+L,  process(Ls,V,Newacc).
