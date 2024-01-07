import Data.List (transpose)

type EmptyCount = Int

type RoundCount = Int

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ls = lines content
  let soln = pushNorth ls
  print $ totalNorthLoad soln

totalNorthLoad :: [[Char]] -> Int
totalNorthLoad grid = sum $ map totalLoad (rotR grid)

totalLoad :: String -> Int
totalLoad str = sum [n | (c, n) <- zip str [1 ..], c == 'O']

pushNorth :: [[Char]] -> [[Char]]
pushNorth = rotL . pushEast . rotR

pushEast :: [[Char]] -> [[Char]]
pushEast = map pushR

pushR :: String -> String
pushR str = pushRLoop str (0, 0) []

pushRLoop :: String -> (EmptyCount, RoundCount) -> String -> String
pushRLoop [] (eC, rC) acc = acc ++ replicate eC '.' ++ replicate rC 'O'
pushRLoop (x : xs) (eC, rC) acc = case x of
  '.' -> pushRLoop xs (eC + 1, rC) acc
  'O' -> pushRLoop xs (eC, rC + 1) acc
  '#' -> pushRLoop xs (0, 0) (acc ++ replicate eC '.' ++ replicate rC 'O' ++ "#")

rotL :: [[a]] -> [[a]]
rotL = reverse . transpose

rotR :: [[a]] -> [[a]]
rotR = transpose . reverse
