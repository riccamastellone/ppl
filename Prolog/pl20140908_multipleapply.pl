% Define a procedure called multiple-apply which takes another procedure f, a natural number n and an item x, and applies f n times to x, i.e. it should return fn(x).
multipleApple(_,0,O,O) :- !.
multipleApply(F,N,X,O) :- N1 is N - 1, call(F,X,Y), !, multipleApple(F,N1,Y,O).
