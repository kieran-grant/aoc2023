import Data.Char (isDigit)
import System.IO

strToInt :: [Char] -> Int
strToInt x = read x :: Int

getCalibration :: [Char] -> [Char]
getCalibration [] = []
getCalibration x = [head x, last x]

lineToInt :: [Char] -> Int
lineToInt = strToInt . getCalibration . filter isDigit

main = do
    contents <- readFile "input1.txt"
    let ls = lines contents
    let result =  sum (map lineToInt ls)
    print result

