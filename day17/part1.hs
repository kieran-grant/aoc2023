import Data.Array.Unboxed
import Data.Char (digitToInt)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified Data.PQueue.Prio.Min as PQ

type Coord = (Int, Int)

type Grid = UArray Coord Int

main = do
    contents <- readFile "sample.txt"
    let iArr = (toGrid . charGridToIntGrid . lines) contents
    let (_, goalNode) = bounds iArr

    let result = aStarSearch (0, 0) (== goalNode) (getNeighbors iArr) (manhattan goalNode)
    print result

getNeighbors :: Grid -> Coord -> [(Coord, Int)] -- neighbors and their costs (g(n))
getNeighbors grid (row, col) = [(n, grid ! n) | n <- neighbors]
  where
    neighbors = filter (not . outOfBounds grid) [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]

outOfBounds :: Grid -> Coord -> Bool
outOfBounds grid (row, col) = row < 0 || row > rowBounds || col < 0 || col > colBounds
  where
    (_, (rowBounds, colBounds)) = bounds grid

manhattan :: Coord -> Coord -> Int -- h(n)
manhattan (y1, x1) (y2, x2) = abs (x1 - x2) + abs (y1 - y2)

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

-- from https://gist.github.com/abhin4v/8172534
aStarSearch :: Coord -> (Coord -> Bool) -> (Coord -> [(Coord, Int)]) -> (Coord -> Int) -> Maybe (Int, [Coord])
aStarSearch startNode isGoalNode nextNodeFn heuristic =
    astar
        (PQ.singleton (heuristic startNode) (startNode, 0))
        Set.empty
        (Map.singleton startNode 0)
        Map.empty
  where
    astar pq seen gscore tracks
        | PQ.null pq = Nothing
        | isGoalNode node = Just (gcost, findPath tracks node)
        | Set.member node seen = astar pq' seen gscore tracks
        | otherwise = astar pq'' seen' gscore' tracks'
      where
        (node, gcost) = snd . PQ.findMin $ pq
        pq' = PQ.deleteMin pq
        seen' = Set.insert node seen
        successors =
            filter
                ( \(s, g, _) ->
                    not (Set.member s seen')
                        && (not (s `Map.member` gscore) || g < (fromJust . Map.lookup s $ gscore))
                )
                $ successorsAndCosts node gcost
        pq'' = foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) pq' successors
        gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors
        tracks' = foldl' (\m (s, _, _) -> Map.insert s node m) tracks successors
    successorsAndCosts node gcost = map (\(s, g) -> (s, gcost + g, heuristic s)) . nextNodeFn $ node
    findPath tracks node =
        if Map.member node tracks
            then findPath tracks (fromJust . Map.lookup node $ tracks) ++ [node]
            else [node]
