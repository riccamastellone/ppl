% Define a Prolog predicate that, given a generic term t, returns a list containing all the atomic elements that appear in t at least twice.
% E.g. given a(1,b(2),1,a(3,b)), it must return [a,1,b].

duplicates(Tree,X) :- treeToList(Tree,Y), onlydup(Y,X).
treeToList(Atom, [Atom]) :- atomic(Atom), !.
treeToList(Tree, [X|Xs]) :- Tree =.. [X|Args], maplist(treeToList,Args,Ys), flatten(Ys,Xs).
% defined also in Exercise 3 of the exam of 2013.02.13
onlydup([],[]).
onlydup([Y|Xs],[Y|Ys]) :- member(Y,Xs), onlydup(Xs,Ys).
onlydup([X|Xs],Ys) :- \+ member(X,Xs), onlydup(Xs,Ys).
