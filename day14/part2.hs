import Data.List (transpose)

type EmptyCount = Int

type RoundCount = Int

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ls = lines content
  print $ solve ls

solve :: [[Char]] -> Int
solve grid = cy !! idx
  where
    loads = map totalNorthLoad (iterate spinCycle grid)
    (st, cy) = findCycle loads
    idx = (1000000000 - length st) `mod` length cy

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

totalNorthLoad :: [[Char]] -> Int
totalNorthLoad grid = sum $ map totalLoad (rotR grid)

totalLoad :: String -> Int
totalLoad str = sum [n | (c, n) <- zip str [1 ..], c == 'O']

spinCycle :: [[Char]] -> [[Char]]
spinCycle grid = iterate rotAndPush grid !! 4

rotAndPush :: [[Char]] -> [[Char]]
rotAndPush = pushEast . rotR

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

rotR :: [[a]] -> [[a]]
rotR = transpose . reverse
