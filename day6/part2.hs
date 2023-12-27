import Data.List (transpose)
import Data.List.Split (splitOn, splitOneOf)

main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let times = map parseTimes ls
  print $ waysToBeat times

waysToBeat :: [Int] -> Int
waysToBeat (secs : rec : _) = length $ filter (willBeat secs rec) [0 .. secs]

willBeat :: Int -> Int -> Int -> Bool
willBeat secs rec curr = curr * (secs - curr) > rec

parseTimes :: String -> Int
parseTimes line = strToInt $ concat $ words $ last $ splitOn ": " line

strToInt :: String -> Int
strToInt str = read str :: Int
