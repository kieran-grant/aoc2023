import Data.Char (digitToInt, toUpper)
import Data.List (tails)

type Coord = (Int, Int)

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Ord)

data Instruction = Instruction
  { dir :: Direction,
    steps :: Int
  }
  deriving (Show, Eq)

main = do
  contents <- readFile "input.txt"
  let instructions = map parseLine (lines contents)
  let (path, perimLen) = followPath instructions
  let solution = perimLen + picksTheorem (shoelace path) perimLen
  print solution

followPath :: [Instruction] -> ([Coord], Int) -- vertices and perimeter length
followPath =
  foldl
    ( \(pathAcc, perim) inst ->
        let res = follow (head pathAcc) inst
         in (fst res : pathAcc, perim + snd res)
    )
    ([(0, 0)], 0)

follow :: Coord -> Instruction -> (Coord, Int)
follow s (Instruction d l) = (addDirection d s l, l)

addDirection :: Direction -> Coord -> Int -> Coord
addDirection North (row, col) n = (row - n, col)
addDirection East (row, col) n = (row, col + n)
addDirection South (row, col) n = (row + n, col)
addDirection West (row, col) n = (row, col - n)

parseLine :: String -> Instruction
parseLine = toInstruction . words

toInstruction :: [String] -> Instruction
toInstruction (_ : _ : c : _) = Instruction d n
  where
    trimmed = (reverse . drop 1 . reverse . drop 2) c
    d = case last trimmed of
      '0' -> East
      '1' -> South
      '2' -> West
      '3' -> North
    n = (hexToDecimal . take 5) trimmed

hexToDecimal :: String -> Int
hexToDecimal = sum . zipWith (*) (iterate (* 16) 1) . reverse . map (digitToInt . toUpper)

picksTheorem :: Int -> Int -> Int
picksTheorem area perimeter = area - (perimeter `quot` 2) + 1

shoelace :: [Coord] -> Int
shoelace path =
  abs $ sum [(y1 + y2) * (x1 - x2) | (y1, x1) : (y2, x2) : _ <- tails path] `quot` 2
