import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)
import System.IO

strToInt :: [Char] -> Int
strToInt x = read x :: Int

parseNum :: String -> Maybe Char
parseNum str
  | (c : _) <- str, isDigit c = Just c
  | "one" `isPrefixOf` str = Just '1'
  | "two" `isPrefixOf` str = Just '2'
  | "three" `isPrefixOf` str = Just '3'
  | "four" `isPrefixOf` str = Just '4'
  | "five" `isPrefixOf` str = Just '5'
  | "six" `isPrefixOf` str = Just '6'
  | "seven" `isPrefixOf` str = Just '7'
  | "eight" `isPrefixOf` str = Just '8'
  | "nine" `isPrefixOf` str = Just '9'
  | otherwise = Nothing

calibration :: String -> Int
calibration line = strToInt [head numbers, last numbers]
  where
    numbers = mapMaybe parseNum (tails line)

main = do
  contents <- readFile "input1.txt"
  let ls = lines contents
  let interim = map calibration ls
  -- let soln = sum $ map calibration ls
  print $ sum interim
