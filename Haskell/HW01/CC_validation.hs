-- http://www.seas.upenn.edu/~cis194/hw/01-intro.pdf
--
-- Homework 1 - Credit Card validation with Luhn algorithm
-- https://en.wikipedia.org/wiki/Luhn_algorithm

module CC_validation where

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

toRevDigits :: Integer -> [Integer]
toRevDigits x | x <= 0 = []
              | otherwise = ( lastDigit x : toRevDigits (dropLastDigit x) )

-- Not used, but as a reminder
-- toDigits :: Integer -> [Integer]
-- toDigits n = reverse (toRevDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs) = ( x : double xs )

double :: [Integer] -> [Integer]
double [] = []
double (x:xs) = ( 2*x : doubleEveryOther xs )

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) | x >= 10 = sumDigits (toRevDigits x ++ xs)
                 | otherwise = x + sumDigits xs


luhn :: Integer -> Bool
luhn x = ( sumDigits (doubleEveryOther (toRevDigits x) ) ) `mod` 10 == 0
