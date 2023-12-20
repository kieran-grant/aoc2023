import Data.Char (digitToInt, isSpace)
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (mapMaybe)

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
    contents <- readFile "sample.txt"
    let ls = lines contents
    let paddedLs = addPadding ls
    mapM print paddedLs

-- let ls = lines contents
-- print $ sum $ map (getMaxCubes . parseCubes) ls
