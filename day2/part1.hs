import Data.Char (digitToInt, isSpace)
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (mapMaybe)

data Cube = Red Int | Green Int | Blue Int

strToInt :: [Char] -> Int
strToInt x = read x :: Int

trimLeft :: String -> String
trimLeft = dropWhile isSpace

isValid :: Cube -> Bool
isValid (Red n) = n <= 12
isValid (Green n) = n <= 13
isValid (Blue n) = n <= 14

createCube :: String -> Int -> Maybe Cube
createCube "red" n = Just (Red n)
createCube "green" n = Just (Green n)
createCube "blue" n = Just (Blue n)
createCube _ _ = Nothing

-- instance Show Cube where
--     show (Red n) = "Red " ++ show n
--     show (Green n) = "Green " ++ show n
--     show (Blue n) = "Blue " ++ show n

strToCube :: String -> Maybe Cube
strToCube str = createCube x (strToInt y)
  where
    x = last wds
    y = head wds
    wds = words str

extractGameAndLines :: String -> (Int, [Cube])
extractGameAndLines line = (x, y)
  where
    x = strToInt $ last . words $ head zs
    y = mapMaybe (strToCube . trimLeft) (splitOneOf ",;" $ last zs)
    zs = splitOn ": " line

getValid :: (Int, [Cube]) -> Maybe Int
getValid (x, cbs)
    | all isValid cbs = Just x
    | otherwise = Nothing

main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    print $ sum $ mapMaybe (getValid . extractGameAndLines) ls
