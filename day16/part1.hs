import Data.Array.Unboxed
import Data.List (nub)
import Data.MemoTrie (memo2)

data Direction = North | East | South | West deriving (Show, Eq)

type Coord = (Int, Int)

type CharArray = UArray Coord Char

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let cArr = toArray $ lines contents
    let energized = buildPath cArr [] (0, 0) East
    (print . length . nub) energized

buildPath :: CharArray -> [(Coord, Direction)] -> Coord -> Direction -> [Coord] -- alter this function to maintain a list of todo states, do the todo at the end
buildPath cArr visited curr dir =
    let next = move curr dir
        newVisited = (curr, dir) : visited
        f = buildPath cArr newVisited next
     in if outOfBounds cArr next || ((curr, dir) `elem` visited)
            then curr : [x | (x, _) <- visited]
            else case (cArr ! next, dir) of
                ('/', _) -> f (turnR dir)
                ('\\', _) -> f (turnL dir)
                ('-', North) -> f West ++ f East
                ('-', South) -> f West ++ f East
                ('|', East) -> f North ++ f South
                ('|', West) -> f North ++ f South
                _ -> f dir

outOfBounds :: CharArray -> Coord -> Bool -- check if move would go out of bounds
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
