import Data.List (sort)
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

instance Ord Mapping where
  (Mapping s1 _) `compare` (Mapping s2 _) = start s1 `compare` start s2

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = (getLineGroups . lines) contents
  let seeds = getSeedIntervals $ getRawSeeds $ head ls
  let mapData = map getData (tail ls)
  let maps = map createMaps mapData
  print seeds
  print (solve seeds maps)

-- start seed intervals, mappings for each level -> lowest location
solve :: [Interval] -> [[Mapping]] -> Int
solve ivals [] = getMinInterval ivals
solve ivals (ms : mss) = solve (concatMap (mapInterval ms) ivals) mss

-- solve ivals (ms : mss) = concatMap (mapInterval ms) ivals

mapInterval :: [Mapping] -> Interval -> [Interval]
mapInterval mps ival = mapInterval' ival (sort $ filter (mappingOverlaps ival) mps) []

mapInterval' :: Interval -> [Mapping] -> [Interval] -> [Interval]
mapInterval' ivl [] [] = [ivl] -- case where no mapping covers interval, just do id
mapInterval' _ [] ivls = ivls -- case where both lists are empty
mapInterval' ivl (m : ms) [] = mapInterval' ivl ms (mapStart ivl m) -- start case
mapInterval' ivl [m] ivls = ivls ++ mapEnd ivl m -- end case
mapInterval' ivl (m : ms) ivls =
  -- mapping conatained inside interval
  let mappedInterval = applyMap m
      fillGap = Interval (end $ interval m) (start (interval $ head ms) - 1)
   in mapInterval' ivl ms (fillGap : mappedInterval : ivls)

applyMap :: Mapping -> Interval
applyMap m = Interval (shift m + start (interval m)) (shift m + end (interval m))

mapStart :: Interval -> Mapping -> [Interval]
mapStart ivl mp =
  let mappingInterval = interval mp
   in if start ivl < start mappingInterval -- if there is some interval before the fist map
        then [Interval (start ivl) (start mappingInterval - 1), applyMap mp] -- add id map to start, then apply map to rest
        else [Interval (start ivl + shift mp) (min (end mappingInterval) (end ivl) + shift mp)] -- otherwise apply map from start of interval to min(end interval, end mapping)

mapEnd :: Interval -> Mapping -> [Interval]
mapEnd ivl mp =
  let mappingInterval = interval mp
   in if end ivl > end (interval mp) -- if there is still some interval left after the last mapping
        then [applyMap mp, Interval (end (interval mp) + 1) (end ivl)] -- apply the map, and add id map rest
        else [Interval (max (start mappingInterval) (start ivl) + shift mp) (end ivl + shift mp)] -- otherwise apply map from max (start map, start ivl) up to end of interval

getMinInterval :: [Interval] -> Int
getMinInterval ivls = minimum $ map start ivls

mappingOverlaps :: Interval -> Mapping -> Bool
mappingOverlaps i m = intervalsOverlap i (interval m)

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
