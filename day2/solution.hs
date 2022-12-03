import System.IO
import System.Environment

data Symbol = Rock | Paper | Scissors deriving(Show)
data Result = Win | Draw | Loss deriving(Show)

main = do
    args <- getArgs
    let file = head args
    putStrLn ("reading from file " ++ file)
    content <- readFile file
    let linesOfFiles = lines content
    let matches1 = map (\[x, y] -> ((mapSymbol x, mapSymbol y)))(map words linesOfFiles)
    let matches2 = map(\[x, y] -> ((mapSymbol x, mapResult y)))(map words linesOfFiles)
    let matchResults1 = map evaluateMatch matches1
    let matchResults2 = map evaluateMatchWithResult matches2
    let result1 = foldl (\acc el -> (((fst acc) + (fst el)), ((snd acc) + (snd el)))) (0, 0) matchResults1
    let result2 = sum matchResults2
    print ("End result should be " ++ show result1)
    print ("result2 " ++ show result2)


mapSymbol :: String -> Symbol 
mapSymbol x = 
    case x of
    "A" -> Rock
    "B" -> Paper
    "C" -> Scissors
    "X" -> Rock
    "Y" -> Paper
    "Z" -> Scissors

mapResult :: String -> Result
mapResult x =
    case x of
    "X" -> Loss 
    "Y" -> Draw
    "Z" -> Win

evaluateSymbol :: Symbol -> Int
evaluateSymbol Rock = 1
evaluateSymbol Paper = 2
evaluateSymbol Scissors = 3

evaluateResult :: Result -> Int
evaluateResult Win = 6
evaluateResult Draw = 3
evaluateResult Loss = 0


evaluateMatchup :: Symbol -> Symbol -> (Int, Int)
evaluateMatchup Rock Rock = (3, 3)
evaluateMatchup Paper Paper = (3, 3)
evaluateMatchup Scissors Scissors = (3, 3)
evaluateMatchup Paper Rock  = (6, 0)
evaluateMatchup Rock Scissors = (6, 0)
evaluateMatchup Scissors Paper = (6, 0)
evaluateMatchup x y = do
    let result = evaluateMatchup y x
    (snd result, fst result)

evaluateMatch :: (Symbol, Symbol) -> (Int, Int)
evaluateMatch (x, y) = do
    let matchup = evaluateMatchup x y
    ((evaluateSymbol x) + (fst matchup), (evaluateSymbol y) + (snd matchup))

evaluateMatchWithResult :: (Symbol, Result) -> (Int)
evaluateMatchWithResult (x, y) = do
    let resultValue = evaluateResult y
    let resultSymbol = getResponseSymbolForResult x y
    (evaluateSymbol resultSymbol) + resultValue

getResponseSymbolForResult :: Symbol -> Result -> Symbol
getResponseSymbolForResult x Draw = x
getResponseSymbolForResult Rock Win = Paper 
getResponseSymbolForResult Rock Loss = Scissors
getResponseSymbolForResult Paper Win = Scissors 
getResponseSymbolForResult Paper Loss = Rock 
getResponseSymbolForResult Scissors Win = Rock 
getResponseSymbolForResult Scissors Loss = Paper 

