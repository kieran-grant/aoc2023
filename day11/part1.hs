import Data.List (tails, transpose)

type Coord = (Int, Int)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ls = lines content
  let emptyRows = getEmpty ls
  let emptyCols = getEmpty $ transpose ls
  let galaxies = findCoordinates '#' ls
  let galaxyPairs = getPairs galaxies
  let distanceFunc = getDistance emptyRows emptyCols
  print $ sum $ map distanceFunc galaxyPairs

getDistance :: [Int] -> [Int] -> (Coord, Coord) -> Int
getDistance emptyRows emptyCols ((y1, x1), (y2, x2)) =
  let l1Distance = manhattan (y1, x1) (y2, x2)
      rowExpansions = getExpansion emptyRows y1 y2
      colExpansions = getExpansion emptyCols x1 x2
   in l1Distance + rowExpansions + colExpansions

getExpansion :: [Int] -> Int -> Int -> Int
getExpansion emp n1 n2 =
  let smaller = min n1 n2
      larger = max n1 n2
   in length $ [x | x <- emp, (x > smaller) && (x < larger)]

manhattan :: Coord -> Coord -> Int
manhattan (y1, x1) (y2, x2) = abs (x1 - x2) + abs (y1 - y2)

getPairs :: [Coord] -> [(Coord, Coord)]
getPairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

findCoordinates :: Char -> [[Char]] -> [(Int, Int)]
findCoordinates targetChar grid =
  concatMap (\(rowIndex, row) -> [(rowIndex, colIndex) | (colIndex, char) <- zip [0 ..] row, char == targetChar]) $
    zip [0 ..] grid

getEmpty :: [[Char]] -> [Int]
getEmpty cArr = [i | (row, i) <- zip cArr [0 ..], all (== '.') row]
