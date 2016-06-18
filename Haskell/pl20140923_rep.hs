-- Define a procedure called rep which takes a list L of elements and returns a list of the elements of L that are repeated at least twice.

rep :: (Eq a) => [a] -> [a]
rep l = reph l []
        where reph (x:xs) l | elem x xs = if (elem x l) then (reph xs l) else (reph xs (l ++ [x]))
                            | otherwise = reph xs l
              reph _ l = l



-- Define a predicate comprep for comparing lists, declaring its type. The predicate must accept another predicate (e.g. <=) and use it to compare the lists. The lists are compared counting the number of duplicated elements in them:
-- e.g. comprep((<=), [1,2,1,2], [0,0,1,0]) is false.

comprep :: (Eq a, Eq b) => (Int -> Int -> Bool) -> [a] -> [b] -> Bool
comprep f l1 l2 = f (length (rep l1)) (length (rep l2))
