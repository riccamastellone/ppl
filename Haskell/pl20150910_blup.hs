-- Define a class called Blup, for a generic type T having two parameters x and y, providing two operations
-- called fisto and fosto. fisto takes a value belonging to T and returns a value of type Maybe x, while fosto
-- takes a value belonging to T and returns a value of type Maybe y.

class Blup where
    fisto :: (a b c) -> Maybe a
    fosto :: (a b c) -> Maybe b

-- Define the sum type Blargh with two parameters of types a and b. It has three data constructor: either Bip
-- with two parameters of types respectively a and b, or Bop with only one parameter of type a, or Bup with no
-- parameters.
-- Make Blargh an instance of class Blup, where fisto is used to access to data of type a, and fosto to
-- data of type b.

data Blargh a b = Bip a b | Bop a | Bup
    deriving(Eq,Show)

instance Blup Blargh where
    fisto (Bip a b) = Just a
    fisto (Bop a) = Just a
    fisto (Bup) = Nothing
    fosto (Bip a b) = Just b
    fosto _ = Nothing

-- Define the sum type Blarf with two parameters of types a and b. It has two data constructor: either La and
-- a list of elements of type a, or Lb and a list of elements of type b.
-- Make Blarf an instance of class Blup, where fisto is used to access to the head of the list of elements
-- of type a, and fosto to the head of the list of elements of type b.

data Blarf a b = La a | Lb b

instance Blup Blarf where
    fisto (La (a:as)) = Just a
    fisto _ = Nothing
    fosto (Lb (b:bs)) = Just b
    fosto _ = Nothing

-- Define a function smap that takes an infinite list L of Int, a function f from Int to Int, an operation OP
-- over Int, and a threshold T. smap performs a map of f on L, while keeping an accumulator K (with
-- starting value 0), which is updated at each step as oldAccumulatorValue OP f(currentElementOfL).
-- smap stops when the value of K reaches T and returns a list of all the computed values of the map.
-- E.g. smap (^2) (+) [1,2..] 100 is the list [1,4,9,16,25,36,49].
-- Write smap’s type.

smap :: (Int -> Int) -> (Int -> Int -> Int) -> [Int] -> Int -> [Int]
smap f op list threshold = smaph f op list threshold 0

smaph f op (x:xs) t k | k < t = fx : smaph f op xs (op k fx)
                      | otherwise = []
                        where fx = (f x)
smaph f _ [x] _ _ = f x
