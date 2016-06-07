%  Define the prefix predicate that holds iff its second argument is a prefix of the first argument.
%  E.g. prefix("Hello world", "Hello") is true, while prefix("Hello world", "wor") is not.

prefix(S,T) :- atom_chars(S,Sl), atom_chars(T,Tl), consume(Sl,Tl).

consume([X|Sl],[X|Tl]) :- !, consume(Sl,Tl).
consume(_,[]).

%  Define an analogous predicate for suffixes.
%  E.g. suffix("Hello world", "world") is true, while suffix("Hello world", "Hello") is not.

suffix(S,T) :- atom_chars(S,Sl), atom_chars(T,Tl), reverse(Sl,Sl2), reverse(Tl,Tl2), consume(Sl2,Tl2).

% Define the infix predicate that holds iff its second argument is a substring of the first argument.
% (Hint: an infix is a prefix of a suffix.)

% Does not work
infix(S,T) :- suffix(S, St), prefix(St, Y).
