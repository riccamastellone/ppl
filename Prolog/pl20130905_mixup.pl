% Define a mixUp predicate, which takes an arithmetic expression without variables containing only the * and
% + operators, and puts in its second argument the value of the expression obtained by changing all the *
% operators to + and vice versa, but keeping the same syntactic structure.
% E.g. mixUp(3*2+4,X) must return X = 20.
% Note: use cut to avoid unnecessary backtracks.

mixUp(V,V) :- atomic(V), !.
mixUp(A1+A2,R) :- !, mixUp(A1,R1), mixUp(A2,R2), R is R1*R2.
mixUp(A1*A2,R) :- !, mixUp(A1,R1), mixUp(A2,R2), R is R1+R2.
