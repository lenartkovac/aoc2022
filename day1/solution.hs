import System.IO
import Data.List

main = do
    putStrLn "insert file"
    file <- getLine
    putStrLn ("reading from file " ++ file)
    content <- readFile file
    let linesOfFiles = lines content
    let groups = getElfCalories linesOfFiles
    let result1 = maxOfList groups
    let result2 = maxThreeElfCaloriesSum groups
    print("Max elf calories", result1)
    print("3 biggest elf calories", result2)


maxThreeElfCaloriesSum :: [Int] -> Int
maxThreeElfCaloriesSum x = sum $ take 3 $ reverse $ sort x

getElfCalories :: [String] -> [Int]
getElfCalories x = do
    let groupsTouple = foldl calculateGroups ([], 0) x
    ((snd groupsTouple):(fst groupsTouple))

getElfCategoriesTouple :: [String] -> ([Int], Int)
getElfCategoriesTouple = foldl calculateGroups ([], 0)

processFile :: Handle -> IO ()
processFile handle = do
    contents <- hGetLine handle
    putStr contents

calcMaxGroup :: (Int, Int) -> String -> (Int, Int)
calcMaxGroup (m, c) [] = ((max m c), 0)
calcMaxGroup (m, c) val = (m, c + (read val :: Int))

calculateGroups :: ([Int], Int) -> String -> ([Int], Int)
calculateGroups (a, c) [] = (c:a, 0)
calculateGroups (a, c) val = (a, c + (getInt val))

getInt :: String -> Int
getInt a = (read a :: Int)

maxOfList :: Ord a => [a] -> a
maxOfList = foldl1 (\x y -> if x >= y then x else y)