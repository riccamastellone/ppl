-- Define a Tree data structure, where each node contains a value and can have any number of children.

data Tree a = Leaf a | Node a [Tree a] 

-- Define a visit function, that returns a list of all the elements that are contained in the tree data structure
-- defined before (you can choose any order you like).

visit :: Tree a -> [a]
visit a = visit' a []
          where visit' (Node v t) l = foldl (++) (l ++ [v]) (map visit t)
                visit' (Leaf v) l = (l ++ [v])

-- Two trees are considered equal iff they contain the same elements and those are in the order defined by
-- the vist function defined before (so they could be structurally different). Define == for Tree.

instance (Eq a) => Eq (Tree a) where
    Leaf a == Leaf b = a == b
    Node a l1 == Node b l2 = a == b && l1 == l2
    _ == _ = False

-- Define the zipToList :: [(a,a)] -> [a] function, that, given a list of pairs, returns a flat list containing
-- all the elements found in the pairs. E.g. zipToList [(1,2),(3,4)] is [1,2,3,4].

zipToList :: [(a,a)] -> [a]
zipToList ((x1,x2):xs) = x1:x2:(zipToList xs)
zipToList [] = []

-- Define an infinite list containing all the elements of the free monoid {a, b}
-- ∗
-- (i.e. all the strings defined on the
-- alphabet {a, b}, empty string included).

fm = "" : zipToList [(x ++ "a", x ++ "b") | x <- fm]
