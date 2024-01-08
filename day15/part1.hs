import Data.Char (ord)
import Data.List.Split (splitOn)

main = do
  content <- readFile "input.txt"
  let input = splitOn "," $ head $ lines content
  print $ solve input

solve :: [[Char]] -> Int
solve input = sum $ map hash input

hash :: [Char] -> Int
hash = foldl (\acc c -> ((acc + ord c) * 17) `mod` 256) 0
