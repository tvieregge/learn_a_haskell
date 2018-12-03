doubleSmallNum x = if x > 100
                      then x
                      else x*2

oddResult = [ x | x <- [1,2,3], x <- [1,2,3]]
