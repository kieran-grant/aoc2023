import Data.List (intersect, transpose)
import Data.List.Split (splitOn)

main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let winners = map (getNumWinners . getNumbersAndWinners . parseCards) ls
  let soln = solver winners
  print soln

getNumWinners :: ([String], [String]) -> Int
getNumWinners x = length $ getIntersection x

solver :: [Int] -> Int
solver xs = solver' xs [] 0

solver' :: [Int] -> [[Int]] -> Int -> Int
solver' [] _ acc = acc
solver' (x : xs) toAdd acc =
  let currentStack = if null toAdd then 1 else sum (head toAdd) + 1
      newYs = update (if null toAdd then [] else tail toAdd) currentStack x
   in solver' xs newYs (acc + currentStack)

update :: [[Int]] -> Int -> Int -> [[Int]]
update ys currVal numWinners = transpose $ replicate numWinners currVal : transpose ys

getIntersection :: ([String], [String]) -> [String]
getIntersection (xs, ys) = xs `intersect` ys

getNumbersAndWinners :: String -> ([String], [String])
getNumbersAndWinners line = (words $ head x, words $ last x)
  where
    x = splitOn " | " line

parseCards :: String -> String
parseCards line = last $ splitOn ": " line
