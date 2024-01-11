import Algorithm.Search (aStar, pruning)
import Data.Array.Unboxed
import Data.Char (digitToInt)
import Data.Maybe (fromJust)

type Coord = (Int, Int)

data Direction = North | East | South | West deriving (Show, Eq, Ord)

data Node = Node
  { position :: Coord,
    direction :: Direction,
    consecutive :: Int
  }
  deriving (Show, Ord, Eq)

type Grid = UArray Coord Int

main = do
  contents <- readFile "input.txt"
  let iArr = (toGrid . charGridToIntGrid . lines) contents
  let result = findMinHeatLoss 4 10 iArr
  print result

findMinHeatLoss :: Int -> Int -> Grid -> Int
findMinHeatLoss min max grid = fst . fromJust . aStar getNext getCost h isGoal $ start
  where
    getNext = getNeighbors min max `pruning` (outOfBounds grid . position)
    getCost _ ns = grid ! position ns
    isGoal m = position m == end && min <= consecutive m
    h = manhattan end

    (_, end) = bounds grid
    start = Node (0, 0) East 0

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

getNeighbors :: Int -> Int -> Node -> [Node] -- neighbors and their costs (g(n))
getNeighbors min max (Node pos dir n)
  | continue = filter ((== dir) . direction) neighbors
  | otherwise = neighbors
  where
    neighbors =
      filter ((<= max) . consecutive)
        . map (\x -> Node (addDirection x pos) x (updateConsec x))
        . filter (/= opposite dir)
        $ [North, South, East, West]
    updateConsec dir'
      | dir' == dir = n + 1
      | n <= 0 = n + 1 -- start node
      | otherwise = 1
    continue = 0 < n && n < min

outOfBounds :: Grid -> Coord -> Bool
outOfBounds grid (row, col) = row < 0 || row > rowBounds || col < 0 || col > colBounds
  where
    (_, (rowBounds, colBounds)) = bounds grid

manhattan :: Coord -> Node -> Int -- h(n)
manhattan (y1, x1) (Node (y2, x2) _ _) = abs (x1 - x2) + abs (y1 - y2)

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
