import Data.Bifunctor (first)
import Data.Char (digitToInt, toUpper)
import Data.List (tails)
import GHC.Driver.Pipeline (fullPipeline)

type Coord = (Int, Int)

type Colour = String

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Ord)

data Instruction = Instruction
  { dir :: Direction,
    steps :: Int,
    colour :: Colour
  }
  deriving (Show, Eq)

data Trench = Trench
  { loc :: Coord,
    col :: Colour
  }
  deriving (Show, Eq)

main = do
  contents <- readFile "input.txt"
  let instructions = map parseLine (lines contents)
  let (path, e) = followPath instructions
  let fullPath = map loc path
  let solution = picksTheorem (shoelace fullPath) (length fullPath)
  print (solution + length fullPath)

followPath :: [Instruction] -> ([Trench], Coord)
followPath =
  foldl (\(pth, l) inst -> let res = follow l inst in first (pth ++) res) ([], (0, 0))

follow :: Coord -> Instruction -> ([Trench], Coord)
follow s i = (res, (loc . last) res)
  where
    res = follow' s i

follow' :: Coord -> Instruction -> [Trench]
follow' _ (Instruction _ 0 _) = []
follow' last (Instruction d n c) =
  Trench next c : follow' next (Instruction d (n - 1) c)
  where
    next = addDirection d last

addDirection :: Direction -> Coord -> Coord
addDirection North (row, col) = (row - 1, col)
addDirection East (row, col) = (row, col + 1)
addDirection South (row, col) = (row + 1, col)
addDirection West (row, col) = (row, col - 1)

parseLine :: String -> Instruction
parseLine = toInstruction . words

toInstruction :: [String] -> Instruction
toInstruction (_ : _ : c : _) = Instruction d n c
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
