import Data.Array.Unboxed
import Data.List (elemIndices)
import Data.Maybe (catMaybes)

data Direction = North | East | South | West

type Coord = (Int, Int)

type CharArray = UArray Coord Char

main = do
  contents <- readFile "input.txt"
  let cArr = toArray $ lines contents
  let (start, dir) = getStart cArr
  let path = buildPath cArr start dir []
  -- print $ reverse path
  print $ length path `quot` 2

getStart :: CharArray -> (Coord, Direction)
getStart cArr =
  let startCoords = findElementCoords cArr 'S'
      surroundingPipes = findSurroundingPipes cArr startCoords
   in (startCoords, head surroundingPipes)

-- getStart cArr =

findSurroundingPipes :: CharArray -> Coord -> [Direction]
findSurroundingPipes cArr start =
  catMaybes $ [findNorth cArr, findEast cArr, findSouth cArr, findWest cArr] <*> pure start

findNorth :: CharArray -> Coord -> Maybe Direction
findNorth cArr start =
  let nextCoord = move start North
      ch = cArr ! nextCoord
   in if ch `elem` "7|F"
        then Just North
        else Nothing

findEast :: CharArray -> Coord -> Maybe Direction
findEast cArr start =
  let nextCoord = move start East
      ch = cArr ! nextCoord
   in if ch `elem` "7-J"
        then Just East
        else Nothing

findSouth :: CharArray -> Coord -> Maybe Direction
findSouth cArr start =
  let nextCoord = move start South
      ch = cArr ! nextCoord
   in if ch `elem` "J|L"
        then Just South
        else Nothing

findWest :: CharArray -> Coord -> Maybe Direction
findWest cArr start =
  let nextCoord = move start West
      ch = cArr ! nextCoord
   in if ch `elem` "L-F"
        then Just West
        else Nothing

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

nextDir :: Char -> Direction -> Direction
nextDir ch dir = case ch of {}

move :: Coord -> Direction -> Coord
move (row, col) dir = case dir of
  North -> (row - 1, col)
  West -> (row, col - 1)
  South -> (row + 1, col)
  East -> (row, col + 1)

-- Util functions

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
        (x : xs) -> length x
      bounds = ((0, 0), (rows - 1, cols - 1))
   in listArray bounds $ concat lists
