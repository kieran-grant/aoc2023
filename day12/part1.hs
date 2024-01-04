import Control.Monad (replicateM)
import Data.List (elemIndices, group)
import Data.List.Split (splitOn)

replaceChars :: Char -> [Char] -> String -> [String]
replaceChars ch replacements str =
  let indices = elemIndices ch str -- get indices of ?
      combs = replicateM (length indices) replacements -- get all combinations of replacements
   in map (replaceAtIndex str indices) combs -- create a string per replacement

replaceAtIndex :: String -> [Int] -> [Char] -> String
replaceAtIndex str indices replacements =
  foldl (\acc (i, r) -> replaceAt i r acc) str (zip indices replacements)

replaceAt :: Int -> Char -> String -> String
replaceAt i r str =
  take i str ++ [r] ++ drop (i + 1) str

stringCombinations :: String -> [String]
stringCombinations = replaceChars '?' ['.', '#']

valid :: [Int] -> String -> Bool
valid ints str = subsequenceLengths str == ints

subsequenceLengths :: String -> [Int]
subsequenceLengths str = map length (filter (elem '#') (group str))

parse :: String -> (String, [Int])
parse str = (head wds, toIntList $ last wds)
  where
    wds = words str

toIntList :: String -> [Int]
toIntList str = map (\x -> read x :: Int) (splitOn "," str)

getValids :: String -> Int
getValids str = length $ filter (valid ints) $ stringCombinations records
  where
    (records, ints) = parse str

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ls = lines content
  let soln = map getValids ls
  print $ sum soln
