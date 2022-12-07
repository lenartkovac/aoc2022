
import System.IO
import System.Environment
import Data.List as List

main = do
    args <- getArgs
    let file = head args
    putStrLn ("reading from file " ++ file)
    content <- readFile file
    let linesOfFile = lines content
    let signal = head linesOfFile
    --print signal

    let result1 = 4 + (resolveMaybe $ find (\x -> unique $ getSection 4 signal x) [0..((length signal) - 4)])
    print result1
    let result2 = 14 + (resolveMaybe $ find (\x -> unique $ getSection 14 signal x) [0..((length signal))])
    print result2


resolveMaybe :: Maybe Int -> Int
resolveMaybe (Just x) = x
resolveMaybe Nothing = -1


getSection :: Int -> [a] -> Int -> [a]
getSection x arr d = take x $ drop d arr 

unique :: Eq a => [a] -> Bool
unique arr =  (length arr) == (length $ List.nub arr)
