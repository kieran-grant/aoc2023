import Data.Array.Unboxed
import Data.List (tails, transpose)

type Coord = (Int, Int)

type CharArray = UArray Coord Char

main = do
  content <- readFile "input.txt"
  let ls = lines content
  -- let cArr = toArray ls
  let emptyRows = getEmpty ls
  let emptyCols = getEmpty $ transpose ls
  let cArr = toArray ls
  let galaxies = findElementCoords cArr '#'
  let galaxyPairs = getPairs galaxies
  let expansionFactor = 1000000
  print $ sum $ map (getDistance expansionFactor emptyRows emptyCols) galaxyPairs

getDistance :: Int -> [Int] -> [Int] -> (Coord, Coord) -> Int
getDistance factor emptyRows emptyCols ((y1, x1), (y2, x2)) =
  let l1Distance = manhattan (y1, x1) (y2, x2)
      rowExpansions = getExpansion emptyRows y1 y2
      colExpansions = getExpansion emptyCols x1 x2
   in l1Distance + (rowExpansions * (factor - 1)) + (colExpansions * (factor - 1))

getExpansion :: [Int] -> Int -> Int -> Int
getExpansion emp n1 n2 =
  let smaller = min n1 n2
      larger = max n1 n2
   in length $ [x | x <- emp, (x > smaller) && (x < larger)]

manhattan :: Coord -> Coord -> Int
manhattan (y1, x1) (y2, x2) = abs (x1 - x2) + abs (y1 - y2)

getPairs :: [Coord] -> [(Coord, Coord)]
getPairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

findElementCoords :: CharArray -> Char -> [Coord]
findElementCoords cArr target =
  let ((minRow, minCol), (maxRow, maxCol)) = bounds cArr
      indices = [(i, j) | i <- [minRow .. maxRow], j <- [minCol .. maxCol], cArr ! (i, j) == target]
   in indices

getEmpty :: [[Char]] -> [Int]
getEmpty cArr = [i | (row, i) <- zip cArr [0 ..], all (== '.') row]

toArray :: [[Char]] -> CharArray
toArray lists =
  let rows = length lists
      cols = case lists of
        [] -> 0
        (x : _) -> length x
      bounds = ((0, 0), (rows - 1, cols - 1))
   in listArray bounds $ concat lists
