import Data.List.Split
import System.IO

fixIndex :: String -> Int
fixIndex inp = (read inp::Int) - 1

validatePasswords :: String -> Bool
validatePasswords line = 
    let feilds = words line
        letter = feilds !! 1 !! 0
        passwd = feilds !! 2 
        limits = splitOn "-" (feilds !! 0)
    in ((passwd !! (fixIndex $ limits !! 0) == letter) /= (passwd !! (fixIndex $ limits !! 1) == letter))

main = do
    handle <- openFile "inp.txt" ReadMode
    contents <- hGetContents handle
    let res = filter (== True) $ map validatePasswords (lines contents) 
    print (length res)
    hClose handle
