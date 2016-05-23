% Define a predicate with one argument to check if a given string is a palindrome.
% E.g. palindrome("sator arepo tenet opera rotas") should return true.
palindrome(S) :- string_to_list(S,X), reverse(X,Y), X == Y.
