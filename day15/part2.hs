import Data.Array
import Data.Char (ord)
import Data.List (elemIndex)
import Data.List.Split (splitOn)

data Lens = Lens
  { label :: String,
    focalLength :: Int
  }
  deriving (Show)

instance Eq Lens where
  x == y = label x == label y

type HashMap = Array Int [Lens]

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = splitOn "," $ head $ lines content
  print $ solve input

solve :: [[Char]] -> Int
solve input =
  let initialMap = listArray (0, 255) (replicate 256 [])
      updatedMap = foldl handleString initialMap input
   in sum $ map (calculatePower updatedMap) [0 .. 255]

calculatePower :: HashMap -> Int -> Int
calculatePower hMap box =
  let vals = hMap ! box
      input = zip vals [1 ..]
      power = map (focussingPower box) input
   in sum power

focussingPower :: Int -> (Lens, Int) -> Int
focussingPower boxNumber (l, m) = (1 + boxNumber) * m * focalLength l

handleString :: HashMap -> String -> HashMap
handleString hashMap str
  | '=' `elem` str = addLens hashMap (parseEqual str)
  | '-' `elem` str = tryRemove hashMap (init str)
  | otherwise = error $ "Unknown HASHMAP operation: " ++ str

addLens :: HashMap -> Lens -> HashMap
addLens hMap l =
  let bucket = hash (label l)
      oldVal = hMap ! bucket
      newVal = add oldVal l
   in hMap // [(bucket, newVal)]

add :: [Lens] -> Lens -> [Lens]
add [] l = [l]
add xs l = case l `elemIndex` xs of
  Just n -> take n xs ++ [l] ++ drop (n + 1) xs
  _ -> xs ++ [l]

tryRemove :: HashMap -> String -> HashMap
tryRemove hMap key =
  let bucket = hash key
      oldVal = hMap ! bucket
      newVal = remove oldVal key
   in hMap // [(bucket, newVal)]

remove :: [Lens] -> String -> [Lens]
remove [] _ = []
remove (x : xs) str =
  if label x == str
    then xs
    else x : remove xs str

parseEqual :: String -> Lens
parseEqual str = Lens l v
  where
    (l, v) = splitEqual str

splitEqual :: String -> (String, Int)
splitEqual str = case elemIndex '=' str of
  Just n ->
    let (h, l) = splitAt n str
     in (h, read (tail l) :: Int)
  _ -> error $ "'=' not in " ++ str

hash :: [Char] -> Int
hash = foldl (\acc c -> ((acc + ord c) * 17) `mod` 256) 0
