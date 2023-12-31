import Data.Array.Unboxed
import Data.List (tails)
import Data.Maybe (mapMaybe)

data Direction = North | East | South | West

type Coord = (Int, Int)

type CharArray = UArray Coord Char

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let cArr = toArray $ lines contents
  let (start, dir) = getStart cArr
  let path = buildPath cArr start dir []
  let solution = picksTheorem (shoelace path) (length path)
  print solution

picksTheorem :: Int -> Int -> Int
picksTheorem area perimeter = area - (perimeter `quot` 2) + 1

shoelace :: [Coord] -> Int
shoelace path =
  abs $ sum [(y1 + y2) * (x1 - x2) | (y1, x1) : (y2, x2) : _ <- tails path] `quot` 2

getStart :: CharArray -> (Coord, Direction)
getStart cArr =
  let startCoords = findElementCoords cArr 'S'
      surroundingPipes = findSurroundingPipes cArr startCoords
   in (startCoords, head surroundingPipes)

findSurroundingPipes :: CharArray -> Coord -> [Direction]
findSurroundingPipes cArr start =
  mapMaybe (find cArr start) [North, East, South, West]

find :: CharArray -> Coord -> Direction -> Maybe Direction
find cArr start dir =
  let nextCoord = move start dir
      ch = cArr ! nextCoord
   in if ch `elem` getValidPipes dir
        then Just dir
        else Nothing

getValidPipes :: Direction -> String
getValidPipes North = "7|F"
getValidPipes East = "-7J"
getValidPipes South = "|JL"
getValidPipes West = "-FL"

findElementCoords :: CharArray -> Char -> Coord
findElementCoords cArr target =
  let ((minRow, minCol), (maxRow, maxCol)) = bounds cArr
      indices = [(i, j) | i <- [minRow .. maxRow], j <- [minCol .. maxCol], cArr ! (i, j) == target]
   in head indices

buildPath :: CharArray -> Coord -> Direction -> [Coord] -> [Coord]
buildPath cArr curr dir path =
  let next = move curr dir
      nextChar = cArr ! next
   in case nextChar of
        'S' -> curr : path
        _ -> handleTurn cArr next dir (curr : path)

handleTurn :: CharArray -> Coord -> Direction -> [Coord] -> [Coord]
handleTurn cArr next dir path =
  case cArr ! next of
    'J' -> buildPath cArr next (turnR dir) path
    'F' -> buildPath cArr next (turnR dir) path
    '7' -> buildPath cArr next (turnL dir) path
    'L' -> buildPath cArr next (turnL dir) path
    _ -> buildPath cArr next dir path

move :: Coord -> Direction -> Coord
move (row, col) dir = case dir of
  North -> (row - 1, col)
  West -> (row, col - 1)
  South -> (row + 1, col)
  East -> (row, col + 1)

turnR :: Direction -> Direction -- 7/L
turnR North = East
turnR East = North
turnR South = West
turnR West = South

turnL :: Direction -> Direction -- J/F
turnL North = West
turnL West = North
turnL South = East
turnL East = South

toArray :: [[Char]] -> CharArray
toArray lists =
  let rows = length lists
      cols = case lists of
        [] -> 0
        (x : _) -> length x
      bounds = ((0, 0), (rows - 1, cols - 1))
   in listArray bounds $ concat lists
