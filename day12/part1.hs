import Data.List.Split (splitOn)

parse :: String -> (String, [Int])
parse str = (head wds, toIntList $ last wds)
  where
    wds = words str

toIntList :: String -> [Int]
toIntList str = map (\x -> read x :: Int) (splitOn "," str)

getValids :: String -> Int
getValids str = countWays records ints
  where
    (records, ints) = parse str

main = do
  content <- readFile "sample.txt" -- ???.### 1,1,3
  let ls = lines content
  print $ last ls
  let soln = map getValids ls
  mapM print soln

countWays :: String -> [Int] -> Int
countWays [] [] = 1 -- if line length === 0 and runs length === 0 return 1
countWays [] _ = 0 -- if line length === 0 and runs length > 0 return 0
countWays xs [] = if '#' `elem` xs then 0 else 1 -- if run lengths == 0, check whether rest of line contains #
countWays (c : cs) (n : ns) =
  if length (c : cs) < sum (n : ns) + length (n : ns) - 1 -- can't fit run
    then 0
    else case c of
      '.' -> countWays cs (n : ns)
      '#' ->
        if ('.' `elem` take (n - 1) cs) || ((n - 1 < length cs) && ((c : cs) !! n == '#'))
          then 0
          else countWays (drop n cs) ns
      '?' -> countWays ('.' : cs) (n : ns) + countWays ('#' : cs) (n : ns)

-- if n == 1
--   then
--     if not (null cs) && head cs == '#'
--       then 0
--       else countWays cs ns
--   else countWays cs ((n - 1) : ns)
