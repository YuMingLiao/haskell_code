module Permutations where
import Data.Permute

permutations :: Permute -> [[Int]]
permutations p = elems p : maybe [] permutations (next p)

to :: [a] -> [Int] -> [a]
to as (i:is) = as !! i : to as is
to _ [] = []

permutationsOf :: [a] -> [[a]]
permutationsOf as = to as <$> permutations ( permute (length as))
