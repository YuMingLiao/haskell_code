module Group1 where
import qualified Data.Choose as C
import Data.List (nub,sort,(\\))
import Control.Arrow hiding (first)
import Prelude hiding (filter)
import Data.Tuple.Extra



data Group a = Nil
             | Cons C.Choose (Group a) [a] --choose from
  deriving (Eq,Show) 

group n (k:ks) as | n >= k && n > 0 && k > 0 = Cons (C.choose n k) (group (n-k) ks (drop k as)) as
group n [] as     | n > 0 = Cons (C.choose n n) Nil as
group n [] as     | n == 0 = Nil

elems :: Group a -> [[a]]
elems (Cons c g as) = (map (as!!) $ C.elems c) : (elems g)
elems Nil = []

sizes :: Group a -> [Int]
sizes (Cons c g _) = [C.size c] ++ sizes g
sizes Nil = []

next :: Group a -> Maybe (Group a)
next (Cons c g as) =
  case next g of
       Nothing -> case C.next c of
                       Nothing -> Nothing
                       Just c'  -> Just $ Cons c' (group ((C.possible c')-(C.size c')) (sizes g) (removeByIndices as (C.elems c'))) as --rebuild with a C.next c and a new tail
       Just g' -> Just (Cons c g' as)
next Nil = Nothing

removeByIndices :: [a] -> [Int] -> [a]
removeByIndices as is = map (as!!) ([0..length as -1] \\ is)
{-
filter p as grp = let res = to as . elems $ grp
                  in (if p res then ([res]:) else id) (maybe [] (filter p as) (next grp))
                                              
allCombsC :: C.Choose -> [[Int]]
allCombsC c = C.elems c : maybe [] allCombsC (C.next c)

allCombs :: Group -> [[[Int]]]
allCombs g = elems g : maybe [] allCombs (next g)

to :: [a] -> [[Int]] ->[[a]]
to xs iss = map (map (xs!!)) iss

chooseKFrom :: Int -> [a] -> [[a]]
chooseKFrom k as = to as $ allCombsC (C.choose (length as) k)

-- two irrevelant chooses 
twoChoose c1 c2 = [ [p1]++[p2] | p1 <- allCombsC c1, p2 <- allCombsC c2]

complement' :: Eq a => [a] -> [a] -> [a]
complement' (x:xs) ys = (if x `notElem` ys then (x:) else id) $ complement' xs ys
complement' [] _ = []

--wholesome
main1 = do
  let xs = "abcde"
  let c1 = chooseKFrom 3 xs
  let complements =  map (complement' xs) c1
  let c2s = chooseKFrom 1 <$> complements
  print $ c2s!!0
  print $ c2s

groupBy xs (k:ks) = [take k xs] ++ groupBy (drop k xs) ks
groupBy xs [] = []
--next
main2 = do
  let xs = "abcde" :: String
  let rep = (take $ length xs) [1..]
  let c1 = C.choose 5 3
  let c1e = C.elems $ C.choose 5 3
  let cmpl = C.complement c1
  
  let c1n = C.next $ C.choose 5 3   
  let g = group 10 [4,3,2]
  print c1
  print c1e
  print cmpl
  print c1n
  print g
  print $ elems g
  print $ next g
  print $ maybe [] (elems) (next g)
  print $ allCombs g


main3 = do
  let xs = [1,1,2,2,2,3,3,3,3,3]
  let cars = [5,3,2]
  let forall = flip map
  let g = group 10 cars
  --print $ to xs $ elems g 
  --print $ map nub $ flip groupBy cars $ to xs $ head (allCombs g) 
  
  print $ forall (allCombs g) (to xs >>> flip groupBy cars >>> map nub >>> shortest) & head --min by

(&) = (flip ($))

shortest :: Ord a => [a] -> [a]
shortest = sort

-}