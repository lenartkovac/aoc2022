import System.IO
import System.Environment

main = do
    args <- getArgs
    let file = head args
    putStrLn ("reading from file " ++ file)
    content <- readFile file
    let linesOfFile = lines content
    let pairs = map splitPairs linesOfFile
    -- part 1
    let processedPairs1 = map (processPair isRangeFullyContained) pairs
    let result1 = sum processedPairs1
    print result1
    -- Part 2
    let processedPairs2 = map (processPair rangesOverlap) pairs
    let result2 = sum processedPairs2
    print result2

isRangeFullyContained :: ((Int, Int), (Int, Int)) -> Int
isRangeFullyContained ((r1a, r1b), (r2a, r2b))
    | r1a <= r2a && r1b >= r2b = 1
    | r2a <= r1a && r2b >= r1b = 1
    | otherwise = 0

rangesOverlap :: ((Int, Int), (Int, Int)) -> Int
rangesOverlap ((r1a, r1b), (r2a, r2b))
    | r1a < r2a && r1b < r2a = 0
    | r1a > r2b && r1b > r2b = 0
    | otherwise = 1

processPair :: (((Int, Int), (Int, Int)) -> Int) -> (String, String) -> Int
processPair rangeFunc pair = rangeFunc $ getRanges pair

getRanges :: (String, String) -> ((Int, Int), (Int, Int))
getRanges (x, y) = (stringToRange x, stringToRange y)

stringToRange :: String -> (Int, Int)
stringToRange str = ((read (rangeEnds!!0) :: Int), (read (rangeEnds!!1) :: Int))
    where rangeEnds = splitStringOnChar '-' str

splitPairs :: String -> (String, String)
splitPairs x = (pairs!!0, pairs!!1)
    where pairs = splitStringOnChar ',' x

splitStringOnChar :: Char -> String -> [String]
splitStringOnChar c [] = []
splitStringOnChar c str = (takeWhile (/=c) str):(splitStringOnChar c $ drop 1 $ dropWhile (/=c) str)
