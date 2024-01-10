import Data.Array.Unboxed
import Data.List (nub)

data Direction = North | East | South | West deriving (Show, Eq)

type Coord = (Int, Int)

type CharArray = UArray Coord Char

main :: IO ()
main = do
    contents <- readFile "sample.txt"
    let cArr = toArray $ lines contents
    let energized = buildPath cArr (0, 0) East
    let stub = take 1000 energized
    let oneIndexed = [((x + 1, y + 1), dir) | ((x, y), dir) <- stub]
    let (st, cy) = findCycle oneIndexed

    print $ nub oneIndexed
    print st
    print cy

buildPath :: CharArray -> Coord -> Direction -> [(Coord, Direction)]
buildPath cArr curr dir =
    let next = move curr dir
     in if outOfBounds cArr next
            then []
            else case (cArr ! next, dir) of
                ('/', _) -> (curr, dir) : buildPath cArr next (turnR dir)
                ('\\', _) -> (curr, dir) : buildPath cArr next (turnL dir)
                ('-', North) -> (curr, dir) : horizontalSplit cArr next
                ('-', South) -> (curr, dir) : horizontalSplit cArr next
                ('|', East) -> (curr, dir) : verticalSplit cArr next
                ('|', West) -> (curr, dir) : verticalSplit cArr next
                _ -> (curr, dir) : buildPath cArr next dir

outOfBounds :: CharArray -> Coord -> Bool -- check if move would go out of bounds
outOfBounds cArr (row, col) =
    let (_, (rowBounds, colBounds)) = bounds cArr
     in row < 0 || row > rowBounds || col < 0 || col > colBounds

horizontalSplit :: CharArray -> Coord -> [(Coord, Direction)] -- buildPath ... ++ buildPath ...
horizontalSplit cArr next = buildPath cArr next West ++ buildPath cArr next East

verticalSplit :: CharArray -> Coord -> [(Coord, Direction)] -- buildPath ... ++ buildPath ...
verticalSplit cArr next = buildPath cArr next South ++ buildPath cArr next North

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

-- Floyd's cycle detection algorithm (https://wiki.haskell.org/Floyd%27s_cycle-finding_algorithm)
findCycle :: (Eq a) => [a] -> ([a], [a])
findCycle xxs = fCycle xxs xxs
  where
    fCycle (x : xs) (_ : y : ys)
        | x == y = fStart xxs xs
        | otherwise = fCycle xs ys
    fCycle _ _ = (xxs, []) -- not cyclic
    fStart (x : xs) (y : ys)
        | x == y = ([], x : fLength x xs)
        | otherwise = let (as, bs) = fStart xs ys in (x : as, bs)
    fLength x (y : ys)
        | x == y = []
        | otherwise = y : fLength x ys
