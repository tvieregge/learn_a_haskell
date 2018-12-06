import Data.List
import Data.Char

doubleSmallNum x = if x > 100
                      then x
                      else x*2

oddResult = [ x | x <- [1,2,3], x <- [1,2,3]]

pg80q = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
pg80q' = length (takeWhile (<1000) $ scanl1 (+) $ map sqrt [1..]) + 1

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

isIn' :: (Eq a) => [a] -> [a] -> Bool
needle `isIn'` haystack = any (isPrefixOf needle) (tails haystack)

pg93 c offset = chr ((ord c) + offset)
