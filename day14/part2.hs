import Data.List (transpose)

type EmptyCount = Int

type RoundCount = Int

main :: IO ()
main = do
  content <- readFile "sample.txt"
  let ls = lines content
  let soln = hammer spinCycle ls
  mapM_ print soln

totalNorthLoad :: [[Char]] -> Int
totalNorthLoad grid = sum $ map totalLoad (rotR grid)

totalLoad :: String -> Int
totalLoad str = sum [n | (c, n) <- zip str [1 ..], c == 'O']

spinCycle :: [[Char]] -> [[Char]]
spinCycle grid = iterate rotAndPush grid !! 4

rotAndPush :: [[Char]] -> [[Char]]
rotAndPush = pushEast . rotR

pushNorth :: [[Char]] -> [[Char]]
pushNorth = rotL . pushEast . rotR

pushWest :: [[Char]] -> [[Char]]
pushWest = rotL . rotL . pushEast . rotR . rotR

hammer :: (Eq a) => (a -> a) -> a -> a
hammer f x
  | x' == x = x'
  | otherwise = hammer f x'
  where
    x' = f x

-- pushSouth :: [[Char]] -> [[Char]]
-- pushSouth = rotR . pushEast . rotL
--
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
