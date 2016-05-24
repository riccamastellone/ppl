% Define a “triparting” predicate that, given a list L and two pivot values, returns three lists such that the first
% contains all the values of L less than both pivots, the second contains values between the pivots (including
% the extremes), the last contains all the remaining values.

triparting([L|Ls],P1,P2,[L1|L1s],L2,L3) :- L < P1, L < P2, L1 = L, !, triparting(Ls,P1,P2,L1s,L2,L3).
triparting([L|Ls],P1,P2,L1,[L2|L2s],L3) :- L >= P1, L =< P2, L2 = L, !, triparting(Ls,P1,P2,L1,L2s,L3).
triparting([L|Ls],P1,P2,L1,L2,[L3|L3s]) :- L > P1, L > P2, L3 = L, !, triparting(Ls,P1,P2,L1,L2,L3s).
triparting([],_,_,[],[],[]).
