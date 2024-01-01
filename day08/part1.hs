import Data.Map.Strict qualified as M

data Direction = Left | Right deriving (Show)

type NodeMap = M.Map String (String, String)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let instructions = cycle $ parseDirections $ head ls
  let networkMap = M.fromList $ map parseNode (tail $ tail ls)
  print $ traverseNetwork instructions networkMap

traverseNetwork :: [Direction] -> NodeMap -> Int
traverseNetwork dirs network = traverseNetworkLoop dirs network "AAA" 0

traverseNetworkLoop :: [Direction] -> NodeMap -> String -> Int -> Int
traverseNetworkLoop (d : ds) network curr n =
  if curr == "ZZZ"
    then n
    else traverseNetworkLoop ds network nextNode (n + 1)
  where
    nextNode = getNextNode (network M.! curr) d

getNextNode :: (String, String) -> Direction -> String
getNextNode (l, r) dir =
  case dir of
    Main.Left -> l
    Main.Right -> r

-- Simple parser for the given format
parseNode :: String -> (String, (String, String))
parseNode input =
  case break (== '=') input of
    (name, '=' : rest) ->
      let (first, second) = parseTuple rest
       in (trim name, (trim first, trim second))
    _ -> error "Invalid input"

-- Helper function to parse the tuple part
parseTuple :: String -> (String, String)
parseTuple input =
  case break (== ',') (init (tail input)) of
    ('(' : first, ',' : second) -> (first, second)
    _ -> error "Invalid tuple"

-- Helper function to trim leading and trailing spaces
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
