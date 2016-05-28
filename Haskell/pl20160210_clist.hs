-- A simply-linked circular list (called Clist from now on) is a list in which the last node points to the first
-- node (see figure). It is sometimes useful to have a sentinel last node, i.e. a node that does not contain
-- data. The sentinel is used e.g. to check if we have traversed the whole list. An empty list contains only the
-- sentinel node, that points to itself.

-- Define a data structure for Clists with a data declaration. Make Clist an instance of Eq – beware: equality
-- test must always terminate.

data Clist a = Last (Clist a) | Node a (Clist a)

instance (Eq a) => Eq (Clist a) where
 Last _ == Last _ = True
 Node a b == Node c d = (a == c) && (b == d)
 _ == _ = False

instance Show a => Show (Clist a) where
 show (Node a b) = show a ++ ", " ++ show b
 show (Last _) = "That's all folks!"

-- Define two functions list2clist and clist2list, that are used to convert an ordinary list to a Clist, and vice versa.
-- Write their types.

list2clist :: [a] -> Clist a
list2clist [] = let new = Last new
                in new
list2clist (x:xs) = let first = Node x $ list2clist' xs first
                    in first

list2clist' [] first = Last first
list2clist' (x:xs) first = Node x $ list2clist' xs first


clist2list :: Clist a -> [a]
clist2list (Last _) = []
clist2list (Node a b) = a : clist2list b

-- Define cmap, a map operation for Clists. Write its type.
cmap :: (a -> b) -> Clist a -> Clist b
cmap f (Node a b) = let first = Node (f a) (cmap' f b first)
                    in first

cmap' f (Node a b) first = Node (f a) (cmap' f b first)
cmap' _ (Last _) first = Last first
