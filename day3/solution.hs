import System.IO
import System.Environment
import Data.HashSet as HashSet 
import Data.Char (ord)

main = do
    args <- getArgs
    let file = head args
    putStrLn ("reading from file " ++ file)
    content <- readFile file
    let linesOfFile = lines content
    --part 1
    let processedLines = Prelude.map processLine linesOfFile
    let result1 = sum $ Prelude.map evaluateDuplicate processedLines
    print result1
    -- part 2
    let chunks = getChunksOfThree linesOfFile
    let commonElements = Prelude.map processChunk chunks
    let result2 = sum $ Prelude.map evaluateDuplicate commonElements
    print result2


--processChunk :: [String] -> [Char]
processChunk :: [String] -> Char
processChunk chunk = head $ HashSet.toList $ Prelude.foldl1 HashSet.intersection $ Prelude.map HashSet.fromList chunk

getChunksOfThree :: [String] -> [[String]]
getChunksOfThree [] = []
getChunksOfThree arr = [Prelude.take 3 arr] ++ (getChunksOfThree $ Prelude.drop 3 arr)


splitStringInHalf :: String -> (String, String)
splitStringInHalf str = do
    let half = (length str) `div` 2
    (Prelude.take half str, Prelude.drop half str)

--processLine :: String -> Int
processLine :: String -> Char
processLine str = do
    let halves = splitStringInHalf str
    let fstHalfSet = HashSet.fromList $ fst halves
    let sndHalfSet = HashSet.fromList $ snd halves
    head $ HashSet.toList $ HashSet.intersection fstHalfSet sndHalfSet

evaluateDuplicate :: Char -> Int
evaluateDuplicate c
    | c >= 'a' = (ord c) - (ord 'a') + 1
    | otherwise = (ord c) - (ord 'A') + 27