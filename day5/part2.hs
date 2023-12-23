import Data.List (intersect, sort, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

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

main = do
  contents <- readFile "sample.txt"
  let ls = (getLineGroups . lines) contents
  let seeds = getSeedIntervals $ getRawSeeds $ head ls
  let mapData = map getData (tail ls)
  let maps = map createMaps mapData
  print (head seeds)
  mapM print (sort $ filter (mappingOverlaps (head seeds)) (head maps))

-- mapM_ print seeds
-- mapM print maps
-- mapM print $ getNewIntervals (head seeds) (head maps)

-- getNewIntervals :: Interval -> [Mapping] -> [Interval]
-- getNewIntervals i mps = getNewIntervals' i mps []
--
-- getNewIntervals' :: Interval -> [Mapping] -> [Interval] -> [Interval]
-- getNewIntervals' _ [] newInts = newInts
-- getNewIntervals' ivl (m : ms) newInts =
--   if intervalsOverlap ivl (interval m)
--     then getNewIntervals' ivl ms (getIntervalsFromMapping ivl m : newInts)
--     else getNewIntervals' ivl ms newInts
--
-- getIntervalsFromMapping :: Interval -> Mapping -> Interval
-- getIntervalsFromMapping i m =
--   let intStart = max (start i) (start $ interval m)
--       intEnd = min (end i) (end $ interval m)
--       diff = shift m
--    in createInterval (intStart + diff) (intEnd + diff)

mapInterval :: Interval -> [Mapping] -> [Interval]
mapInterval ival mps = mapInterval' ival (sort $ filter (mappingOverlaps ival) mps) []

mapInterval' :: Interval -> [Mapping] -> [Maybe Interval] -> [Interval]
mapInterval' ivl mps [] = mapInterval' ivl mps [mapStart ivl (head mps)] -- start case
mapInterval' ivl [m] ivls = catMaybes $ mapEnd ivl m : ivls -- end case
mapInterval' ivl (m1 : m2 : ms) ivls =
  let mappedInterval = Just (applyMap m1 ivl)
      fillGap = Just (Interval (end $ interval m1) (start (interval m1) - 1))
   in mapInterval' ivl (m2 : ms) (fillGap : mappedInterval : ivls)

applyMap :: Mapping -> Interval -> Interval
applyMap m i = Interval (start i + shift m) (end i + shift m)

mapStart :: Interval -> Mapping -> Maybe Interval
mapStart ivl mp =
  if start ivl < start (interval mp)
    then Just (Interval (start ivl) (start (interval mp) - 1))
    else Nothing

mapEnd :: Interval -> Mapping -> Maybe Interval
mapEnd ivl mp =
  if end ivl > end (interval mp)
    then Just (Interval (end (interval mp)) (end ivl))
    else Nothing

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

inRange :: (Int, Int) -> Int -> Bool
inRange (s, e) n = s <= n && n < e
