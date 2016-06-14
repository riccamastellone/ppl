-- Define the function infixes, which takes a list g as input and returns the list of all infixes (i.e. non-empty contiguous sublists) of g.
-- For instance, infixes "ciao" is the list ["o","ao","iao","ciao","a","ia","cia","i","ci","c"] (remember that a string is a list of characters in Haskell).

-- infixes :: String -> [String]
infixes s =  foldl (++) [] (map (\x -> prefixes x []) (suffixes s)))

prefixes (c:cs) [] = prefixes cs [[c]]
prefixes (c:cs) acc = prefixes cs $ ((head acc) ++ [c]) : acc
prefixes _ acc = acc

suffixes (c:cs) = [c:cs] ++ suffixes cs
suffixes _ = []
