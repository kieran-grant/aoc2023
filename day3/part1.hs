import Data.Char (digitToInt, isAlphaNum, isDigit, isSpace)
import Data.List (findIndices)
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (mapMaybe)

-- padding functions
addPadding :: [String] -> [String]
addPadding toPad = addPadding' toPad []

addPadding' :: [String] -> [String] -> [String]
addPadding' (x : xs) [] = addPadding' (x : xs) [makeRowOfDots $ length x + 2]
addPadding' (x : xs) ys = addPadding' xs (ys ++ [appendDotsToLine x])
addPadding' [] (y : ys) = [y] ++ ys ++ [y]

appendDotsToLine :: String -> String
appendDotsToLine str = '.' : (str ++ ['.'])

makeRowOfDots :: Int -> String
makeRowOfDots n = replicate n '.'

main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    let paddedLs = addPadding ls
    let intsNextToSymbols = consume paddedLs
    -- mapM print $ unique $ concat intsNextToSymbols
    print $ sum $ map parsedIntToInt $ unique $ concat intsNextToSymbols

-- scratch

data ParsedInt = ParsedInt
    { value :: Int
    , startX :: Int
    , endX :: Int
    , row :: Int
    }
    deriving (Show, Eq)

type Symbol = Int

consume :: [String] -> [[ParsedInt]]
consume strings = consume' (zip strings [0 ..]) []

consume' :: [(String, Int)] -> [[ParsedInt]] -> [[ParsedInt]]
consume' ((a, _) : (b, y) : (c, z) : strings) ints = consume' ((b, y) : (c, z) : strings) (ints ++ [getValidParts a b c y])
consume' _ ints = ints

getValidParts :: String -> String -> String -> Int -> [ParsedInt]
getValidParts prev curr next currY =
    let x = getNums prev (currY - 1)
        y = getNums curr currY
        z = getNums next (currY + 1)
     in filter (isSymbolAdj' $ getSymbols curr) $ x ++ y ++ z

isSymbolAdj' :: [Symbol] -> ParsedInt -> Bool
isSymbolAdj' syms n = any (inRange n) syms

inRange :: ParsedInt -> Symbol -> Bool
inRange n sym = (endX n >= (sym - 1)) && (startX n <= (sym + 1))

getSymbols :: String -> [Symbol]
getSymbols = findIndices isSymbol

incrementSymbol :: Symbol -> Symbol
incrementSymbol s = s + 1

isSymbol :: Char -> Bool
isSymbol x = not $ isAlphaNum x || (x == '.')

getNums :: String -> Int -> [ParsedInt]
getNums str y = getNums' str 0 y [] []

-- line -> xPos -> yPos -> numAcc -> numsSoFar -> output
getNums' :: String -> Int -> Int -> String -> [ParsedInt] -> [ParsedInt]
getNums' [] _ _ [] allNums = allNums
getNums' [] x y numAcc allNums = allNums ++ [strToParsedInt numAcc x y]
getNums' (c : cs) x y numAcc allNums
    | isDigit c = getNums' cs (x + 1) y (numAcc ++ [c]) allNums -- current char is digit, add to running number and continue
    | null numAcc = getNums' cs (x + 1) y [] allNums -- current char isn't a digit, and running list is empty, just continue
    | otherwise = getNums' cs (x + 1) y [] (allNums ++ [strToParsedInt numAcc x y]) -- current char isn't a digit, and have accumulated some numbers, turn that into an int

strToParsedInt :: String -> Int -> Int -> ParsedInt
strToParsedInt str x y = ParsedInt{value = strToInt str, startX = x - length str, endX = x - 1, row = y}

strToInt :: [Char] -> Int
strToInt x = read x :: Int

unique [] = []
unique (x : xs) = x : unique (filter (x /=) xs)

parsedIntToInt :: ParsedInt -> Int
parsedIntToInt = value
