% Consider binary trees represented as a hierarchic lists, where each node is a list [node, subtree1, subtree2].
% Leaves are just symbols. In the colored subtree problem, we take as input a tree, and put into each internal
% node a number representing the number of different leaves present in its subtrees.
% E.g. given this tree: [R,[X,yellow,brown],[Y,blue,yellow]] the solution is: R = 3, X = Y = 2.
% Define the col_tree predicate, that solves the colored subtree problem.
% Hint: the predicate union(X,Y,Z) holds if the list Z is the union of X and Y, seen as sets.


col_tree([1, X, X], [X]) :- atomic(X), !.
col_tree([2, X, Y], [X,Y]) :- atomic(X), atomic(Y), !.
col_tree([N, Tree1, Tree2], Colors) :- atomic(Tree1), col_tree(Tree2, Col2), !, union([Tree1], Col2, Colors), length(Colors, N).
col_tree([N, Tree1, Tree2], Colors) :- col_tree(Tree1, Col1), atomic(Tree2), !, union(Col1, [Tree2], Colors), length(Colors, N).
col_tree([N, Tree1, Tree2], Colors) :- col_tree(Tree1, Col1), col_tree(Tree2, Col2), !, union(Col1, Col2, Colors), length(Colors, N).
