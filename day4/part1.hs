import Data.Char (digitToInt, isSpace)
import Data.List (intersect)
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (mapMaybe)

main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    let soln = sum $ map (getScore . getNumbersAndWinners . parseCards) ls
    print soln

getScore :: ([String], [String]) -> Int
getScore x = getPoints $ length $ getIntersection x

getPoints :: Int -> Int
getPoints 0 = 0
getPoints n = 2 ^ (n - 1)

getIntersection :: ([String], [String]) -> [String]
getIntersection (xs, ys) = xs `intersect` ys

getNumbersAndWinners :: String -> ([String], [String])
getNumbersAndWinners line = (words $ head x, words $ last x)
  where
    x = splitOn " | " line

parseCards :: String -> String
parseCards line = last $ splitOn ": " line
