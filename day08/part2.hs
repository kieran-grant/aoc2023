import Data.List (isSuffixOf)
import Data.Map.Strict qualified as M

data Direction = Left | Right deriving (Show)

type NodeMap = M.Map String (String, String)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let instructions = cycle $ parseDirections $ head ls
  let networkMap = M.fromList $ map parseNode (tail $ tail ls)
  print $ lcmm $ traverseNetwork instructions networkMap

traverseNetwork :: [Direction] -> NodeMap -> [Int]
traverseNetwork dirs network = map (traverseNetworkLoop dirs network 0) startNodes
  where
    startNodes = [x | x <- M.keys network, "A" `isSuffixOf` x]

traverseNetworkLoop :: [Direction] -> NodeMap -> Int -> String -> Int
traverseNetworkLoop (d : ds) network n currNode =
  if isEnd currNode
    then n
    else traverseNetworkLoop ds network (n + 1) newNode
  where
    newNode = getNextNode network d currNode

isEnd :: String -> Bool
isEnd = isSuffixOf "Z"

lcmm :: [Int] -> Int
lcmm = foldr lcm 1

getNextNode :: NodeMap -> Direction -> String -> String
getNextNode nodeMap dir curr =
  case dir of
    Main.Left -> fst $ nodeMap M.! curr
    Main.Right -> snd $ nodeMap M.! curr

parseNode :: String -> (String, (String, String))
parseNode input =
  case break (== '=') input of
    (name, '=' : rest) ->
      let (first, second) = parseTuple rest
       in (trim name, (trim first, trim second))
    _ -> error "Invalid input"

parseTuple :: String -> (String, String)
parseTuple input =
  case break (== ',') (init (tail input)) of
    ('(' : first, ',' : second) -> (first, second)
    _ -> error "Invalid tuple"

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

parseDirections :: [Char] -> [Direction]
parseDirections str = parseDirectionsLoop str []

parseDirectionsLoop :: [Char] -> [Direction] -> [Direction]
parseDirectionsLoop [] dirs = dirs
parseDirectionsLoop (x : xs) dirs =
  if x == 'L'
    then parseDirectionsLoop xs (dirs ++ [Main.Left])
    else parseDirectionsLoop xs (dirs ++ [Main.Right])
