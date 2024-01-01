import Data.Char (digitToInt, isSpace)
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (mapMaybe)

data Cube = Red Int | Green Int | Blue Int

strToInt :: [Char] -> Int
strToInt x = read x :: Int

trimLeft :: String -> String
trimLeft = dropWhile isSpace

getMaxCubes :: [Cube] -> Int
getMaxCubes [] = 0
getMaxCubes cbs = product $ getMaxCubes' cbs

getMaxCubes' :: [Cube] -> [Int]
getMaxCubes' cbs = [cubesToMax isRed cbs, cubesToMax isBlue cbs, cubesToMax isGreen cbs]

cubesToMax :: (Cube -> Bool) -> [Cube] -> Int
cubesToMax predicate cubes = maximum (map cubeToInt (filter predicate cubes))

cubeToInt :: Cube -> Int
cubeToInt (Red n) = n
cubeToInt (Blue n) = n
cubeToInt (Green n) = n

isRed :: Cube -> Bool
isRed (Red _) = True
isRed _ = False

isBlue :: Cube -> Bool
isBlue (Blue _) = True
isBlue _ = False

isGreen :: Cube -> Bool
isGreen (Green _) = True
isGreen _ = False

createCube :: String -> Int -> Maybe Cube
createCube "red" n = Just (Red n)
createCube "green" n = Just (Green n)
createCube "blue" n = Just (Blue n)
createCube _ _ = Nothing

-- instance Show Cube where
--   show (Red n) = "Red " ++ show n
--   show (Green n) = "Green " ++ show n
--   show (Blue n) = "Blue " ++ show n

strToCube :: String -> Maybe Cube
strToCube str = createCube x (strToInt y)
  where
    x = last wds
    y = head wds
    wds = words str

extractCubes :: String -> [Cube]
-- "2 red, 3 blue ; 4 green" -> [Red 2, Blue 3, Green 4]
extractCubes cubeStr = mapMaybe (strToCube . trimLeft) (splitOneOf ",;" cubeStr)

parseCubes :: String -> [Cube]
parseCubes line = extractCubes $ last $ splitOn ": " line

main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  print $ sum $ map (getMaxCubes . parseCubes) ls
