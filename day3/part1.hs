import Data.Char (digitToInt, isAlphaNum, isDigit, isSpace)
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
    -- mapM print $ concat intsNextToSymbols
    print $ unique $ concat intsNextToSymbols

-- scratch

consume :: [String] -> [[Int]]
consume strings = consume' strings []

consume' :: [String] -> [[Int]] -> [[Int]]
consume' (a : b : c : strings) ints = consume' (b : c : strings) (ints ++ [checkLine a b c])
consume' _ ints = ints

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

strToInt :: [Char] -> Int
strToInt x = read x :: Int

{-
Good start!!

Next, in the checkLine method, we need to:
    [ ] - modify the isSymbol method to return a list of 1D indices of symbols on the current line
    [ ] - then, for each of these indices, we need to get *adjacent* numbers, e.g. (if x is one of the indicies) getNums currLine [x-1, x+1], getNums prevLine [x-1, x, x+1], getNums [x-1, x, x+1]
        [ ] - could use what I already have, but add an additional check to see if any part of the number that has just been parsed is adjacent, if so append
            - e.g.  c
-}

-- currLine -> indexDecrementer -> runningIntBuffer -> parsedInts
getAdjNums :: String -> Int -> String -> [Int] -> [Int]
getAdjNums _ (-1) [] allNums = allNums

-- need case where we are beyond index (-1) but haven't finished reading int

checkLine :: String -> String -> String -> [Int]
checkLine prev curr next =
    if any isSymbol curr
        then getNums prev ++ getNums curr ++ getNums next
        else []

isSymbol :: Char -> Bool
isSymbol x = not $ isAlphaNum x || (x == '.')

getNums :: String -> [Int]
getNums str = getNums' str [] []

getNums' :: String -> String -> [Int] -> [Int]
getNums' [] [] allNums = allNums
getNums' [] numAcc allNums = allNums ++ [strToInt numAcc]
getNums' (x : xs) numAcc allNums
    | isDigit x = getNums' xs (numAcc ++ [x]) allNums -- current char is digit, add to running number and continue
    | null numAcc = getNums' xs [] allNums -- current char isn't a digit, and running list is empty, just continue
    | otherwise = getNums' xs [] (allNums ++ [strToInt numAcc]) -- current char isn't a digit, and have accumulated some numbers, turn that into an int

-- let ls = lines contents
-- print $ sum $ map (getMaxCubes . parseCubes) ls
--

unique [] = []
unique (x : xs) = x : unique (filter ((/=) x) xs)
