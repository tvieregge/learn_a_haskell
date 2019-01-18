type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (x,y) = filter onBoard nextPos
    where onBoard (x,y) = elem x [1..8] && elem y [1..8]
          nextPos = [(x+2,y-1),(x+2,y+1),(x-2,y-1),(x-2,y+1)
                    ,(x+1,y-2),(x+1,y+2),(x-1,y-2),(x-1,y+2)]
