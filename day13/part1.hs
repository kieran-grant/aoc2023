import Data.List (isPrefixOf, isSuffixOf, transpose)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ls = lines content
  let allGrids = grids ls
  let results = map getReflection allGrids
  let colValues = sum $ mapMaybe fst results
  let rowValues = sum $ mapMaybe snd results

  print $ colValues + 100 * rowValues

getReflection :: [String] -> (Maybe Int, Maybe Int)
getReflection grid = (colRefl, rowRefl)
  where
    colRefl = refl grid
    rowRefl = case colRefl of
      Just _ -> Nothing
      _ -> refl (transpose grid)

refl :: [String] -> Maybe Int
refl grid = reflLoop grid [1 .. length (head grid) - 1]

reflLoop :: [String] -> [Int] -> Maybe Int
reflLoop _ [] = Nothing
reflLoop grid (n : ns) =
  if all (partialPalindrome n) grid then Just n else reflLoop grid ns

partialPalindrome :: Int -> String -> Bool
partialPalindrome n line =
  let (start, end) = splitAt n line
   in if length start > length end
        then reverse end `isSuffixOf` start
        else reverse start `isPrefixOf` end

-- parsing functions
grids :: [String] -> [[String]]
grids xs = gridsLoop xs [] []

gridsLoop :: [String] -> [String] -> [[String]] -> [[String]]
gridsLoop [] [] outStrs = outStrs
gridsLoop [] ys outStrs = outStrs ++ [ys]
gridsLoop (x : xs) ys outStrs =
  if null x
    then gridsLoop xs [] (outStrs ++ [ys])
    else gridsLoop xs (ys ++ [x]) outStrs
