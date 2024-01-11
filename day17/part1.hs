import Data.Array.Unboxed
import Data.Char (digitToInt)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Hashable
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.PQueue.Prio.Min qualified as PQ
import GHC.Data.Graph.UnVar (neighbors)
import GHC.Generics

type Coord = (Int, Int)

data Direction = North | East | South | West deriving (Show, Eq)

instance Hashable Direction where
  hashWithSalt salt North = hashWithSalt salt (0 :: Int)
  hashWithSalt salt East = hashWithSalt salt (1 :: Int)
  hashWithSalt salt South = hashWithSalt salt (2 :: Int)
  hashWithSalt salt West = hashWithSalt salt (3 :: Int)

type Node = (Coord, Direction)

type Grid = UArray Coord Int

main = do
  contents <- readFile "sample.txt"
  let iArr = (toGrid . charGridToIntGrid . lines) contents
  let (_, goalNode) = bounds iArr

  let result = aStarSearch ((0, 0), South) ((== goalNode) . fst) (getNeighbors iArr) (manhattan goalNode)
  print result

getNeighbors :: Grid -> Node -> [(Node, Int)] -- neighbors and their costs (g(n))
getNeighbors grid ((row, col), dir) = [(n, grid ! fst n) | n <- filteredNeighbors]
  where
    neighbors =
      filter
        (not . outOfBounds grid . fst)
        [ ((row + 1, col), South),
          ((row - 1, col), North),
          ((row, col + 1), East),
          ((row, col - 1), West)
        ]
    filteredNeighbors = filter ((/= dir) . snd) neighbors

outOfBounds :: Grid -> Coord -> Bool
outOfBounds grid (row, col) = row < 0 || row > rowBounds || col < 0 || col > colBounds
  where
    (_, (rowBounds, colBounds)) = bounds grid

manhattan :: Coord -> Node -> Int -- h(n)
manhattan (y1, x1) ((y2, x2), _) = abs (x1 - x2) + abs (y1 - y2)

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
aStarSearch :: Node -> (Node -> Bool) -> (Node -> [(Node, Int)]) -> (Node -> Int) -> Maybe (Int, [Node])
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
