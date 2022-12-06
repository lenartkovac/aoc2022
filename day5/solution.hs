import System.IO
import System.Environment
import Data.Map as Map

main = do
    args <- getArgs
    let file = head args
    putStrLn ("reading from file " ++ file)
    content <- readFile file
    let linesOfFile = lines content
    let splitInput = splitInputIntoInitStateAndInstructions linesOfFile
    let initialState = getInitState $ fst splitInput
    let instructions = Prelude.map (\x -> (read (x!!1) :: Int, read (x!!3) :: Int, read (x!!5) :: Int)) $ Prelude.map words $ snd splitInput
    -- part 1
    let finalState1 = Prelude.foldl (\currState instruction -> processInstruction9000 currState instruction) initialState instructions
    let result1 = Prelude.map (\(x, y) -> if (length y) > 0 then head y else ' ') $ Map.toList finalState1
    print result1
    --part 2
    let finalState2 = Prelude.foldl (\currState instruction -> processInstruction9001 currState instruction) initialState instructions
    let result2 = Prelude.map (\(x, y) -> if (length y) > 0 then head y else ' ') $ Map.toList finalState2
    print result2



processInstruction9000 :: Map Int [Char] -> (Int, Int, Int) -> Map Int [Char]
processInstruction9000 currState (a, f, t) = do
    let from = currState!f
    let to = currState!t
    let payload = Prelude.take a from
    let newState = Map.insert f (Prelude.drop a from) currState
    Map.insert t ((reverse payload) ++ to) newState

processInstruction9001 :: Map Int [Char] -> (Int, Int, Int) -> Map Int [Char]
processInstruction9001 currState (a, f, t) = do
    let from = currState!f
    let to = currState!t
    let payload = Prelude.take a from
    let newState = Map.insert f (Prelude.drop a from) currState
    Map.insert t ((payload) ++ to) newState

splitInputIntoInitStateAndInstructions :: [String] -> ([String], [String])
splitInputIntoInitStateAndInstructions x = (initState, instructions)
    where
        initState = takeWhile (/="") x
        instructions = Prelude.drop 1 $ dropWhile (/="") x

getInitState :: [String] -> Map Int [Char]
getInitState x = Prelude.foldl (\mp (s, ct) -> Map.insert s ct mp) Map.empty $ Prelude.map (\(c, i) -> (c, getStackContent stacksContents i)) stacks
    where
        stacksContents = init x
        stacksString = last $ x
        stacks = Prelude.map (\(x, y) -> (read [x] :: Int, y)) $ Prelude.filter (\x -> fst x /= ' ') $ Prelude.zip stacksString [0..]

getStackContent :: [String] -> Int -> [Char]
getStackContent x idx = reverse $ takeWhile (/=' ') $ Prelude.map (\x -> x!!idx) reverseContent
    where 
        reverseContent = reverse x


