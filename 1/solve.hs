import Control.Monad

equals20 :: [Int] -> Int 
equals20 x
        | res == 2020 = foldl (\acc y -> acc * y) 1 x
        | otherwise = 0
        where res = sum(x)

checkbook = [INSERT NUMBERS HERE]

percheck = replicateM 3 checkbook
result = filter (/= 0) $ map (\x -> equals20 x) percheck

main = do
        print result
