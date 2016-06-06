-- Make lists of numbers a instances of the class Num. If the two lists have different length, you must assume
-- that the missing elements of the shorter are all 0.
-- E.g. [1,2,3] * [2,-1] should be [2,-2,0].
-- (Remember that you need to define methods for +, -, *, abs, signum, and fromInteger.)

--instance (Num a) => (Num [a]) where
--    [] + a = a
--    a + [] = a
--    [] - a = -a
--    a - [] = a
--    a * [] = map (\x -> 0) a
--    [] * a = map (\x -> 0) a
--    abs (x:xs) = (abs x):(abs xs)
--    signum a = map signum x
--    fromInteger a = [fromInteger a]
--    (x:xs) + (y:ys) = (x + y):(xs + ys)
--    (x:xs) - (y:ys) = (x - y):(xs - ys)
--    (x:xs) * (y:ys) = (x * y):(xs * ys)


-- Define a recursive data structure of type TT which can be used to represent lists of Int of any depth (e.g. in
-- Scheme ’(1 2 (3 9) ((1) -7))).

data TT = VV Int | LL [TT] deriving (Eq,Show)

-- Define a predicate lile, which, given a TT value, check if it contains its own length.
-- E.g., using a Scheme-like notation (lile ’(2 1)) holds, while (lile ’(1 2 1)) does not.

lile :: TT -> Bool
lile l = member (len l) l

member :: Int -> TT -> Bool
member x (LL l) = elem (VV x) l

len :: TT -> Int
len (VV _) = 0
len (LL x) = length x

-- Define a version of the predicate lile, called lileg, which takes a value of type TT, and check if all the lists
-- in it contain their length.
-- E.g., using a Scheme-like notation: (lileg ’(2 (2 (1)) 3)) must hold.

lileg :: TT -> Bool
lileg l = lile l && iflc l

iflc (LL []) = True
ilfc (LL ((VV x):xs)) = iflc (LL xs)
iflc (LL (x:xs)) = lileg x && iflc (LL xs)
