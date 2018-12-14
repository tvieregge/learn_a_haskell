import System.IO

main = do
    handle <- openFile "temp" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle -- needed when using openFile
