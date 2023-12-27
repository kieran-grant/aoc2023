import Data.List (transpose)
import Data.List.Split (splitOn, splitOneOf)

main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let times = transpose $ map parseTimes ls
  print $ product $ map waysToBeat times

waysToBeat :: [Int] -> Int
waysToBeat (secs : rec : _) = length $ filter (willBeat secs rec) [0 .. secs]

willBeat :: Int -> Int -> Int -> Bool
willBeat secs rec curr = curr * (secs - curr) > rec

parseTimes :: String -> [Int]
parseTimes line = map strToInt $ words $ last $ splitOn ": " line

strToInt :: String -> Int
strToInt str = read str :: Int
