import Data.Char (isDigit)
import System.IO

strToInt :: [Char] -> Int
strToInt x = read x :: Int

getCalibration :: [Char] -> [Char]
getCalibration [] = []
getCalibration x = [head x, last x]

slice :: Int -> Int -> [Char] -> [Char]
slice a b = (take (b - a) . (drop a))

lineToInt :: [Char] -> Int
lineToInt = strToInt . getCalibration . filter isDigit

main = do
    contents <- readFile "day1.txt"
    let result =  (map slice 0 5 ls)
    print result

