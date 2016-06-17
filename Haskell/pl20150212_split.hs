-- Implement split in Haskell, noting that the Scheme version returns a vector: is there a more suitable type in
-- Haskell for the job? If so, use it.

split :: [a] -> Int -> ([a],[a])
split l n = splith l n [] []

splith :: [a] -> Int -> [a] -> [a] -> ([a],[a])
splith (x:xs) n pre pos | n > 0 = splith xs (n - 1) (pre ++ [x]) pos
                        | otherwise = splith xs (n - 1) pre (pos ++ [x])
splith _ _ pre pos = (pre,pos)


-- Implement 3-factors in Haskell, noting that the Scheme version returns a list of lists: is there a more suitable
-- type in Haskell for the job? If so, use it.

f3actors :: [a] -> [([a],[a],[a])]
f3actors l = f3acth l len len
            where len = length l

f3acth :: [a] -> Int -> Int -> [([a],[a],[a])]
f3acth l n1 n2 | n1 > 0 && n2 > 0 = (p1, p2, p3) : f3acth l n1 (n2 - 1)
               | n1 > 0 = (p1, p2, p3) : f3acth l (n1 - 1) (length l)
               | otherwise = []
            where (p12,p3) = split l n1
                  (p1,p2) = split p12 n2


