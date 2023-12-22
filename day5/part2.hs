import Data.List (intersect, transpose)
import Data.List.Split (splitOn)

data Interval = Interval
  { start :: Int,
    end :: Int
  }

data Mapping = Mapping
  { source :: Int,
    dest :: Int,
    range :: Int
  }

main = do
  contents <- readFile "input.txt"
  let ls = (getLineGroups . lines) contents
  let rawSeeds = getRawSeeds (head ls)
  let seeds = getAllSeeds rawSeeds
  print $ length seeds

-- let mapData = map getData (tail ls)
-- let pipeline = makePipeline mapData
-- let soln = getMin pipeline seeds
-- print soln

getMin :: [Int -> Int] -> [Int] -> Int
getMin fns seeds = getMin' fns seeds [0 ..]

getMin' :: [Int -> Int] -> [Int] -> [Int] -> Int
getMin' fns seeds (x : xs) =
  if pipe fns x `elem` seeds
    then x
    else getMin' fns seeds xs

makePipeline :: [[[Int]]] -> [Int -> Int]
makePipeline ns = makePipeline' ns []

makePipeline' :: [[[Int]]] -> [Int -> Int] -> [Int -> Int]
makePipeline' xs fns = foldl (\fns x -> mapMaker x : fns) fns xs

pipe :: [a -> a] -> a -> a
pipe fs a = foldl (flip ($)) a fs -- can change foldl to scanl for better debugging

mapMaker :: [[Int]] -> Int -> Int
mapMaker [] n = n
mapMaker (r : rs) n =
  let sourceStart = head r
      destStart = r !! 1
      rangeLength = last r
   in if inRange (sourceStart, sourceStart + rangeLength) n
        then destStart + (n - sourceStart)
        else mapMaker rs n

getData :: [String] -> [[Int]]
getData strs = map parseInts (tail strs)

parseInts :: String -> [Int]
parseInts line = map strToInt (words line)

getRawSeeds :: [String] -> [Int]
getRawSeeds line = map strToInt (words $ last $ splitOn ": " (head line))

getAllSeeds :: [Int] -> [Int]
getAllSeeds xs = getAllSeeds' xs []

getAllSeeds' :: [Int] -> [Int] -> [Int]
getAllSeeds' [] ys = ys
getAllSeeds' (s : r : xs) ys = getAllSeeds' xs (ys ++ [s .. (s + r - 1)])

getLineGroups :: [String] -> [[String]]
getLineGroups strs = getLineGroups' strs [] []

getLineGroups' :: [String] -> [String] -> [[String]] -> [[String]]
getLineGroups' [] [] outStrs = outStrs
getLineGroups' [] ys outStrs = outStrs ++ [ys]
getLineGroups' (x : xs) ys outStrs =
  if null x
    then getLineGroups' xs [] (outStrs ++ [ys])
    else getLineGroups' xs (ys ++ [x]) outStrs

strToInt :: String -> Int
strToInt str = read str :: Int

inRange :: (Int, Int) -> Int -> Bool
inRange (s, e) n = s <= n && n < e
