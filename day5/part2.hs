import Data.List (intersect, transpose)
import Data.List.Split (splitOn)

data Interval = Interval
  { start :: Int,
    end :: Int
  }
  deriving (Show, Eq)

data Mapping = Mapping
  { interval :: Interval,
    shift :: Int
  }
  deriving (Show, Eq)

main = do
  contents <- readFile "sample.txt"
  let ls = (getLineGroups . lines) contents
  let seeds = getSeedIntervals $ getRawSeeds $ head ls
  let mapData = map getData (tail ls)
  let maps = map createMaps mapData
  -- mapM_ print seeds
  -- mapM print maps
  mapM print $ getNewIntervals (head seeds) (head maps)

getNewIntervals :: Interval -> [Mapping] -> [Interval]
getNewIntervals i mps = getNewIntervals' i mps []

getNewIntervals' :: Interval -> [Mapping] -> [Interval] -> [Interval]
getNewIntervals' _ [] newInts = newInts
getNewIntervals' ivl (m : ms) newInts =
  if intervalsOverlap ivl (interval m)
    then getNewIntervals' ivl ms (getIntervalsFromMapping ivl m : newInts)
    else getNewIntervals' ivl ms newInts

getIntervalsFromMapping :: Interval -> Mapping -> Interval
getIntervalsFromMapping i m =
  let intStart = max (start i) (start $ interval m)
      intEnd = min (end i) (end $ interval m)
      diff = shift m
   in createInterval (intStart + diff) (intEnd + diff)

getMinInterval :: [Interval] -> Int
getMinInterval ivls = minimum $ map start ivls

intervalsOverlap :: Interval -> Interval -> Bool
intervalsOverlap i1 i2 = end i1 >= start i2 && start i1 <= end i2

-- parsing functions

createMaps :: [[Int]] -> [Mapping]
createMaps xss = createMaps' xss []

createMaps' :: [[Int]] -> [Mapping] -> [Mapping]
createMaps' xss mps = foldl (\mps xs -> createMapFromList xs : mps) mps xss

createMapFromList :: [Int] -> Mapping
createMapFromList (dst : src : rng : _) = Mapping (createInterval src rng) (dst - src)

getData :: [String] -> [[Int]]
getData strs = map parseInts (tail strs)

parseInts :: String -> [Int]
parseInts line = map strToInt (words line)

getRawSeeds :: [String] -> [Int]
getRawSeeds line = map strToInt (words $ last $ splitOn ": " (head line))

getSeedIntervals :: [Int] -> [Interval]
getSeedIntervals xs = getSeedIntervals' xs []

getSeedIntervals' :: [Int] -> [Interval] -> [Interval]
getSeedIntervals' [] ys = ys
getSeedIntervals' (x1 : x2 : xs) ys = getSeedIntervals' xs (createInterval x1 x2 : ys)

getLineGroups :: [String] -> [[String]]
getLineGroups strs = getLineGroups' strs [] []

createInterval :: Int -> Int -> Interval
createInterval start range = Interval start (start + range - 1)

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
