import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStrLn "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = reads numberString
        if not $ null number
        then if randNumber == ( fst $ number !! 0 )
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        else putStrLn "didn't catch that..."
        askForNumber newGen
