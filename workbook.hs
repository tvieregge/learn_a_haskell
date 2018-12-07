import qualified Data.Map as Map
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

-- no $ needed at end up second line?
-- phoneBook :: Map.Map String String
-- phoneBook = Map.fromList $
phoneBook =
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("bonnie", "459-2919")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2
