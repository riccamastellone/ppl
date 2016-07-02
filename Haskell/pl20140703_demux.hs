-- Consider a variant of the problem seen in Exercise 1: an unsorted list can contain list of elements of some
-- type, integer numbers, or the special value End.
-- E.g. (in a pseudo-Haskell syntax) [3, [6, 6, 1], 4, [1, 2], -2, End, 9].

-- Define the data structure for the unsorted list, and Demuxed, analogous to the structure introduced in Exercise
-- 1 (i.e. with two fields, one containing an integer value, the sum of the integer elements found, and a list
-- of all the found lists).

data UList a = L [a] | V a | End

data Demuxed a = Demuxed Int [[a]]

-- Define the demux function, that takes an unsorted list l and builds up a demuxed data structure containing
-- the processed data of l (only that before End, if present). demux must be strict (i.e. non lazy).

demux l = demux' l 0 []
		 	where demux' ((V l):ls) n v = let n' = l + n in seq n' demux' ls n' v
		 		  demux' ((L l):ls) n v = let v' = (v ++ l) in seq demux' ls n v'
		 		  demux' (End:ls) n v = Demuxed n v
		 		  demux' [] n v = Demuxed n v
