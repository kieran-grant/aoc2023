import Data.Array.Unboxed

data Direction = North | East | South | West

type Coord = (Int, Int)

type CharArray = UArray Coord Char

main = do
  contents <- readFile "sample1-2.txt"
  let ls = toArray $ lines contents
  let path = buildPath ls (1, 1) East []
  -- print $ reverse path
  print $ length path `quot` 2

buildPath :: CharArray -> Coord -> Direction -> [Coord] -> [Coord]
buildPath cArr curr dir path =
  let next = move curr dir
      nextChar = cArr ! next
   in case nextChar of
        'S' -> curr : path
        'J' -> buildPath cArr next (turnR dir) (curr : path)
        'F' -> buildPath cArr next (turnR dir) (curr : path)
        '7' -> buildPath cArr next (turnL dir) (curr : path)
        'L' -> buildPath cArr next (turnL dir) (curr : path)
        _ -> buildPath cArr next dir (curr : path)

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
