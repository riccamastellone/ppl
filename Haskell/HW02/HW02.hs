{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)
moveGuess :: Move -> Code
moveGuess (Move code _ _) = code

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [x] [y] | x == y = 1
                     | otherwise = 0
exactMatches (x:xs) (y:ys) | x == y = (exactMatches xs ys) + 1
                           | otherwise = (exactMatches xs ys)
exactMatches _ _ = 0

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors = countColors2 colors

countColors2 :: [Peg] -> Code -> [Int]
countColors2 (colour:colours) code = (length (filter (\x -> x == colour) code)) : (countColors2 colours code)
countColors2 _ _ = []

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches code guess = (minSum (countColors code) (countColors guess))

minSum :: [Int] -> [Int] -> Int
minSum (x:xs) (y:ys) | x <= y = x + (minSum xs ys)
                     | otherwise = y + (minSum xs ys)
minSum _ _ = 0

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove code guess = Move guess em (m - em)
    where em = exactMatches code guess
          m = matches code guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move code = (getMove code (moveGuess move)) == move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (\x -> isConsistent move x) codes 

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = [ c : code| c <- colors, code <- allCodes ( n - 1 ) ]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve c = choose c $ allCodes 6

choose :: Code -> [Code] -> [Move]
choose _ [] = []
choose c (x:xs) =
    if x == c then [m] else m : (choose c $ filterCodes m xs)
            where m = getMove c x

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
