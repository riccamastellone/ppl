-- http://www.seas.upenn.edu/~cis194/hw/01-intro.pdf
--
-- Homework 1 - Tower of Hanoi
-- https://en.wikipedia.org/wiki/Tower_of_Hanoi

module Hanoi_tower where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
