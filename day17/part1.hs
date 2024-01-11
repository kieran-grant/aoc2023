import Algorithm.Search (aStar)
import Data.Array.Unboxed
import Data.Char (digitToInt)
import Data.Hashable
import Data.Maybe (fromJust)

type Coord = (Int, Int)

data Direction = North | East | South | West deriving (Show, Eq, Ord)

data Move = Move
  { position :: Coord,
    direction :: Direction,
    consecutive :: Int
  }
  deriving (Show, Ord, Eq)

type Grid = UArray Coord Int

main = do
  contents <- readFile "sample.txt"
  let iArr = (toGrid . charGridToIntGrid . lines) contents
  let (_, goalCoord) = bounds iArr

  let startNode = Move (-1, 0) East (-1)
  let result = aStar (getNeighbors iArr 3) (getNeighborCosts iArr) (manhattan goalCoord) ((== goalCoord) . position) startNode
  print $ (fst . fromJust) result

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = North
opposite West = East

addDirection :: Direction -> Coord -> Coord
addDirection North (row, col) = (row - 1, col)
addDirection East (row, col) = (row, col + 1)
addDirection South (row, col) = (row + 1, col)
addDirection West (row, col) = (row, col - 1)

getNeighborCosts :: Grid -> Move -> Move -> Int
getNeighborCosts grid _ (Move c _ _)
  | c == (0, 0) = 0
  | otherwise = grid ! c

getNeighbors :: Grid -> Int -> Move -> [Move] -- neighbors and their costs (g(n))
getNeighbors grid max (Move pos dir n) = neighbors
  where
    neighbors =
      filter ((<= max) . consecutive)
        . filter (not . outOfBounds grid . position)
        . map (\x -> Move (addDirection x pos) x (updateConsec x))
        . filter (/= opposite dir)
        $ [North, South, East, West]
    updateConsec dir'
      | dir' == dir = n + 1
      | n < 0 = 0 -- start node
      | otherwise = 1

outOfBounds :: Grid -> Coord -> Bool
outOfBounds grid (row, col) = row < 0 || row > rowBounds || col < 0 || col > colBounds
  where
    (_, (rowBounds, colBounds)) = bounds grid

manhattan :: Coord -> Move -> Int -- h(n)
manhattan (y1, x1) (Move (y2, x2) _ _) = abs (x1 - x2) + abs (y1 - y2)

charGridToIntGrid :: [[Char]] -> [[Int]]
charGridToIntGrid = map (map digitToInt)

toGrid :: [[Int]] -> Grid
toGrid lists =
  let rows = length lists
      cols = case lists of
        [] -> 0
        (x : _) -> length x
      bnds = ((0, 0), (rows - 1, cols - 1))
   in listArray bnds $ concat lists
