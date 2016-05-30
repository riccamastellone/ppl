% Define the remove predicate, knowing that remove(Elem, List1, List2) is true when List1, with
% Elem removed, results in List2.
% Example:
% ?- remove(3,[2,3,1,3],X).
% X = [2, 1, 3] ; X = [2, 3, 1]

remove(_,[],[]).
remove(X,[X|L1s],L2) :- remove(X,L1s,L2).
remove(X,[Y|L1s],[Y|L2s]) :- remove(X,L1s,L2s).
