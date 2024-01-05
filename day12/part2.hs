import Data.Function.Memoize (memoize, memoize2)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ls = lines content
  let soln = map getValids ls
  print soln
  print $ sum soln

parse :: String -> (String, [Int])
parse str = (head wds, toIntList $ last wds)
  where
    wds = words str

toIntList :: String -> [Int]
toIntList str = map (\x -> read x :: Int) (splitOn "," str)

getValids :: String -> Int
getValids str = countWays repeatedRecords repeatedInts
  where
    (records, ints) = parse str
    repeatedRecords = init $ concat $ replicate 5 (records ++ ['?'])
    repeatedInts = concat $ replicate 5 ints

countWays :: String -> [Int] -> Int
countWays = memoize2 countWays'
  where
    countWays' [] [] = 1
    countWays' [] _ = 0
    countWays' xs [] = if '#' `elem` xs then 0 else 1
    countWays' k@(c : cs) v@(n : ns) =
      if length (c : cs) < sum (n : ns) + length (n : ns) - 1 -- can't fit run
        then 0
        else case c of
          '.' -> countWays cs (n : ns)
          '?' -> countWays ('.' : cs) (n : ns) + countWays ('#' : cs) (n : ns)
          '#' ->
            if ('.' `elem` take (n - 1) cs) || ((n - 1 < length cs) && (cs !! (n - 1) == '#'))
              then 0
              else countWays (drop n cs) ns
          _ -> error "Not a recognised character!"
