import Data.Char (digitToInt)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing)
import Data.Ord qualified as Ord

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let hands = map parseLine ls
  print $ getTotalWinnings hands

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Ord, Eq)

data Hand = Hand
  { cards :: String,
    bid :: Int,
    handType :: HandType
  }
  deriving (Show, Eq)

instance Ord Hand where
  (Hand h1 _ hT1) `compare` (Hand h2 _ hT2) =
    if hT1 == hT2
      then h1 `compareCards` h2
      else hT1 `compare` hT2

compareCards :: [Char] -> [Char] -> Ordering
compareCards [] [] = EQ
compareCards (x : xs) (y : ys)
  | cardValue x == cardValue y = compareCards xs ys
  | cardValue x < cardValue y = LT
  | cardValue x > cardValue y = GT

cardValue :: Char -> Int
cardValue c = case c of
  'A' -> 14
  'K' -> 13
  'Q' -> 12
  'J' -> 11
  'T' -> 10
  _ -> digitToInt c

parseLine :: String -> Hand
parseLine str = Hand c b t
  where
    w = words str
    c = head w
    b = readStrToInt $ last w
    t = calculateHandType c

calculateHandType :: String -> HandType
calculateHandType str = getHandFromLengths (map length $ group $ sort str)

readStrToInt :: String -> Int
readStrToInt str = read str :: Int

getHandFromLengths :: [Int] -> HandType
getHandFromLengths xs = case sortBy (comparing Ord.Down) xs of
  [5] -> FiveOfAKind
  [4, _] -> FourOfAKind
  [3, 2] -> FullHouse
  [3, _, _] -> ThreeOfAKind
  [2, 2, _] -> TwoPair
  [2, _, _, _] -> OnePair
  _ -> HighCard

getTotalWinnings :: [Hand] -> Int
getTotalWinnings hands = sum $ zipWith (curry multiplyBidByRank) (sort hands) [1 ..]

multiplyBidByRank :: (Hand, Int) -> Int
multiplyBidByRank (h, i) = bid h * i
