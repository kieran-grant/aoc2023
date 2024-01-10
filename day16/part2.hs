import Data.Array.IArray
import Data.Array.Unboxed
import Data.List (nub)

data Direction = North | East | South | West deriving (Show, Eq)

type Coord = (Int, Int)

type Beam = (Coord, Direction)

type CharArray = UArray Coord Char

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let cArr = toArray $ lines contents
    let initialStarts = edges cArr
    (print . maximum) $ map (solve cArr) initialStarts

solve :: CharArray -> Beam -> Int
solve cArr init = (length . nub) energized
  where
    energized = search cArr [] [init]

edges :: CharArray -> [Beam]
edges cArr =
    let (_, (upperRow, upperCol)) = bounds cArr
     in getInitial cArr [((r, 0), East) | r <- [0 .. upperRow]]
            ++ getInitial cArr [((r, upperCol), West) | r <- [0 .. upperRow]]
            ++ getInitial cArr [((0, c), South) | c <- [0 .. upperCol]]
            ++ getInitial cArr [((upperRow, c), North) | c <- [0 .. upperCol]]

getInitial :: CharArray -> [Beam] -> [Beam]
getInitial _ [] = []
getInitial cArr ((c, d) : cs)
    | outOfBounds cArr c = []
    | otherwise = [(c, d') | d' <- nextDir (cArr ! c) d] ++ getInitial cArr cs

search :: CharArray -> [Beam] -> [Beam] -> [Coord]
search _ visited [] = [x | (x, _) <- visited]
search grid visited (x@(pos, dir) : xs)
    | x `elem` visited = search grid visited xs
    | outOfBounds grid nextPos = search grid (x : visited) xs
    | otherwise = search grid (x : visited) (toCheck ++ xs)
  where
    nextPos = move pos dir
    toCheck = [(nextPos, d) | d <- nextDir (grid ! nextPos) dir]

nextDir :: Char -> Direction -> [Direction]
nextDir c dir = case (c, dir) of
    ('/', _) -> [turnR dir]
    ('\\', _) -> [turnL dir]
    ('-', North) -> [West, East]
    ('-', South) -> [West, East]
    ('|', East) -> [North, South]
    ('|', West) -> [North, South]
    _ -> [dir]

outOfBounds :: CharArray -> Coord -> Bool
outOfBounds cArr (row, col) =
    let (_, (rowBounds, colBounds)) = bounds cArr
     in row < 0 || row > rowBounds || col < 0 || col > colBounds

move :: Coord -> Direction -> Coord
move (row, col) dir = case dir of
    North -> (row - 1, col)
    West -> (row, col - 1)
    South -> (row + 1, col)
    East -> (row, col + 1)

turnR :: Direction -> Direction -- /
turnR North = East
turnR East = North
turnR South = West
turnR West = South

turnL :: Direction -> Direction -- \
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
        bnds = ((0, 0), (rows - 1, cols - 1))
     in listArray bnds $ concat lists
