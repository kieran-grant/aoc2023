import Data.Char (isDigit)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.IO

-- Function to create a map from strings to integers
numberMap :: Map.Map String Int
numberMap =
  Map.fromList
    [ ("one", 1),
      ("two", 2),
      ("three", 3),
      ("four", 4),
      ("five", 5),
      ("six", 6),
      ("seven", 7),
      ("eight", 8),
      ("nine", 9)
    ]

-- want to say if first character is digit, or the string starts with one of the above keys, return the corresponding value

getFirstDigit :: T.Text -> Maybe Int
getFirstDigit T.empty = Nothing
getFirstDigit t = [x.Value if (T.isPrefixOf x.Key t) | x <- numberMap]

split :: Int -> Int -> T.Text -> T.Text
split a b = T.take (b - a) . T.drop a

main = do
  contents <- TIO.readFile "day1.txt"
  let ls = T.lines contents
  let soln = map (split 0 3) ls
  print soln

-- let result = sum (map lineToInt ls)
-- print result
