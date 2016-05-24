% Given a pair of lists [x1, x2, . . . , xn] and [y1, y2, . . . , yn], define a deterministic predicate maxsum to obtain the
% maximum value of xk + yk. The two lists are assumed to have the same length.
% 
% Example
% ?- maxsum([3,2,-1],[2,1,7],X).
% X = 6.

maxsum([L1],[L2],Y) :- Y is L1 + L2.
maxsum([L1|L1s],[L2|L2s],Y) :- M is L1 + L2, maxsum(L1s,L2s,Z), Z > M, Y is Z, !.
maxsum([_|L1s],[_|L2s],Y) :- maxsum(L1s,L2s,Y).
