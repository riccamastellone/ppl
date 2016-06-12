-- Assume this is a possible Haskell variant of producer in Exercise 1, in which the closures’ states are made
-- explicit.
producer ag1 ag2 = prod' ag1 [] ag2 0 1
                where
                prod' ag1 st1 ag2 st2 i | i >= 10 = (st2:st1)
                prod' ag1 st1 ag2 st2 i | odd i = prod' ag1 (ag1 i st1) ag2 st2 (i+1)
                prod' ag1 st1 ag2 st2 i | even i = prod' ag1 st1 ag2 (ag2 i st2) (i+1)
-- 1. Define two suitable functions for producer’s two arguments, such that its call returns [20,9,7,5,3,1].
-- 2. Write the type of prod’, assuming that all the numbers have type Int


-- product (:) (+)

-- prod' :: (Int -> [Int] -> [Int]) -> [Int] -> (Int -> Int -> Int) -> Int -> Int -> [Int]


-- Define an higher-order function called duofold, which takes two binary functions f and g, a starting value t
-- and a (finite) list [e1, e2, . . .], and returns . . . f(g(f(t, e1), e2), e3), .... Please, write also its type.
-- Example: duofold (+) (-) 0 [1,2,3,4] returns −2 (i.e. 0 + 1 − 2 + 3 − 4).

duofold :: (t1 -> t -> t1) -> (t1 -> t -> t1) -> t1 -> [t] -> t1
duofold f1 f2 t (x:xs) = duofold f2 f1 (f1 t x) xs
duofold _ _ t _ = t
