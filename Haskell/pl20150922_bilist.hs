-- Define the Bilist data-type, which is a container of two homogeneous lists. Define an accessor for Blist,
-- called bilist_ref, that, given an index i, returns the pair of values at position i in both lists.
-- E.g. bilist_ref (Bilist [1,2,3] [4,5,6]) 1 should return (2,5).

data Bilist a = Bilist [a] [a] deriving (Show, Eq)

bilist_ref :: Bilist a -> Int -> (a,a)
bilist_ref (Bilist l1 l2) index = ( (l1 !! index) , (l2 !! index) )

-- Define a function, called oddeven, that is used to build a Bilist x y from a simple list. oddeven takes all
-- the elements at odd positions and put them in y, while all the other elements are put in x, maintaining their
-- order. You may assume that the given list has an even length (or 0). Write also all the types of the functions
-- you define.
-- E.g. oddeven [1,2,3,4] must be Bilist [1,3] [2,4].

oddeven :: [a] -> Bilist a
oddeven x = oddeven' x [] []

oddeven' :: [a] -> [a] -> [a] -> Bilist a
oddeven' (x:xs) ev od = oddeven' xs od (ev ++ [x])
oddeven' [] ev od = Bilist ev od

-- Define an inverse of oddeven, e.g. inv_oddeven $ oddeven [1,2,3,4] must be [1,2,3,4]. Write also all
-- the types of the functions you define.

inv_oddeven :: Bilist a -> [a]
inv_oddeven (Bilist [] _ ) = []
inv_oddeven (Bilist (x:xs) y) = ( x : (inv_oddeven (Bilist y xs)) )

-- Define a function, called bilist_max, that given an input Bilist [x1, x2, . . . , xn] [y1, y2, . . . , yn], where xk +
-- yk, for 1 ≤ k ≤ n, is the maximum, returns k.
-- E.g.
-- > bilist_max (Bilist [3,2,-1] [2,1,7])
-- 2

bilist_max :: Bilist Int -> Int
bilist_max (Bilist (x:xs) (y:ys)) = bilist_max' (Bilist xs ys) 1 0 (x+y)

bilist_max' (Bilist (x:xs) (y:ys)) pos k max | sum > max = bilist_max' (Bilist xs ys) newpos pos sum
                                             | otherwise = bilist_max' (Bilist xs ys) newpos k max
                                             where sum = x+y
                                                   newpos = pos+1
bilist_max' (Bilist [] _) pos k max = k
