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
    let energized = search cArr [] [((0, 0), South)]

    (print . reverse) [(x + 1, y + 1) | (x, y) <- energized]
    (print . length . nub) energized

search :: CharArray -> [Beam] -> [Beam] -> [Coord]
search _ visited [] = [x | (x, _) <- visited]
search grid visited ((pos, dir) : xs) =
    if (pos, dir) `elem` visited
        then search grid visited xs
        else
            let nextPos = move pos dir
             in if outOfBounds grid nextPos
                    then search grid ((pos, dir) : visited) xs
                    else case (grid ! nextPos, dir) of
                        ('.', _) -> search grid ((pos, dir) : visited) ((nextPos, dir) : xs)
                        ('/', _) -> search grid ((pos, dir) : visited) ((nextPos, turnR dir) : xs)
                        ('\\', _) -> search grid ((pos, dir) : visited) ((nextPos, turnL dir) : xs)
                        ('-', North) -> search grid ((pos, dir) : visited) ((nextPos, West) : (nextPos, East) : xs)
                        ('-', South) -> search grid ((pos, dir) : visited) ((nextPos, West) : (nextPos, East) : xs)
                        ('|', East) -> search grid ((pos, dir) : visited) ((nextPos, North) : (nextPos, South) : xs)
                        ('|', West) -> search grid ((pos, dir) : visited) ((nextPos, North) : (nextPos, South) : xs)
                        _ -> search grid ((pos, dir) : visited) ((nextPos, dir) : xs)

-- ('|', West) -> search grid ((here, dir) : visited) ((nextTile, North) : (nextTile, South) : xs)

{-
            let continueSearch = search grid ((here, dir) : visited)
                nextLoc = move here dir
             in case (grid ! nextLoc, dir) of -- some logic error here, should maybe be next?
                    ('/', _) -> continueSearch ((nextLoc, turnR dir) : xs)
                    ('\\', _) -> continueSearch ((nextLoc, turnL dir) : xs)
                    ('-', North) -> continueSearch ((nextLoc, West) : (nextLoc, East) : xs)
                    ('-', South) -> continueSearch ((nextLoc, West) : (nextLoc, East) : xs)
                    ('|', East) -> continueSearch ((nextLoc, North) : (nextLoc, South) : xs)
                    ('|', West) -> continueSearch ((nextLoc, North) : (nextLoc, South) : xs)
                    _ -> continueSearch ((nextLoc, dir) : xs)
                    -}

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
