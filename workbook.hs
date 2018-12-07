import qualified Data.Map as Map
import Data.List
import Data.Char

-- A bunch more stuff about OO haskell in chapter 7
import Geometry.Sphere

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

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
      Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
      Just (state, code) -> if state /= Taken
                               then Right code
                               else Left $ "Locker " ++ show lockerNumber
                               ++ " is alread taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken, "ZD39I"))
    ,(101,(Free, "JAH3I"))
    ,(103,(Free, "IQSA9"))
    ,(105,(Free, "QOTSA"))
    ,(109,(Taken, "893JJ"))
    ,(110,(Taken, "99292"))
    ]
