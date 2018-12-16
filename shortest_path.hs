data Section = Section { getA :: Int, getB :: Int, getC :: Int }
    deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

graph :: RoadSystem
graph = [ Section 50 10 30
        , Section 5 90 20
        , Section 40 2 25
        , Section 10 8 0
        ]

main = do
    print $ shortest_path graph

shortest_path :: RoadSystem -> Path
shortest_path system =
    let (aPath,bPath) = foldl best_next ([],[]) system
    in if (pathCost aPath) < (pathCost bPath)
          then reverse aPath
          else reverse bPath

best_next :: (Path, Path) -> Section -> (Path, Path)
best_next (aPath, bPath) (Section a b c) =
    let aCost = pathCost aPath
        bCost = pathCost bPath
        pathAA = aCost + a
        pathBCA = bCost + b + c
        pathBB = bCost + b
        pathACB = aCost + a + c
        newPathA = if pathAA <= pathBCA
                      then (A,a):aPath
                      else (C,c):(B,b):aPath
        newPathB = if pathBB <= pathACB
                      then (B,b):bPath
                      else (C,c):(A,a):bPath
     in (newPathA, newPathB)

pathCost :: Path -> Int
pathCost p = sum $ map snd p
