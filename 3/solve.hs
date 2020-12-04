import System.IO

countTrees :: [String] -> Int -> Int -> Int -> Int -> Int -> Int
countTrees treemap x y right down tree_count
  | y >= (length treemap) = tree_count
  | ((treemap !! y) !! (x  `mod` (length $ treemap !! y))) == '#' = countTrees treemap (x + right) (y + down) right down (tree_count + 1)
  | ((treemap !! y) !! (x `mod` (length $ treemap !! y))) == '.' = countTrees treemap (x + right) (y + down) right down tree_count

main = do
    handle <- openFile "inp.txt" ReadMode
    contents <- hGetContents handle
    let mapArray = lines contents
    let treeCount1 = countTrees mapArray 0 0 1 1 0
    let treeCount2 = countTrees mapArray 0 0 3 1 0
    let treeCount3 = countTrees mapArray 0 0 5 1 0
    let treeCount4 = countTrees mapArray 0 0 7 1 0
    let treeCount5 = countTrees mapArray 0 0 1 2 0
    print treeCount1
    print treeCount2
    print treeCount3
    print treeCount4
    print treeCount5
    hClose handle
