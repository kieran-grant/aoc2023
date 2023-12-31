main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let sequences = map lineToSeq ls
  print $ sum $ map getPrediction sequences

getPrediction :: [Int] -> Int
getPrediction seq = getPredictionLoop seq []

getPredictionLoop :: [Int] -> [Int] -> Int
getPredictionLoop currSeq lasts =
  if all (== 0) currSeq
    then sum lasts
    else getPredictionLoop (getNewSequence currSeq) (last currSeq : lasts)

getNewSequence :: [Int] -> [Int]
getNewSequence oldSeq = zipWith subtract oldSeq (tail oldSeq)

strToInt :: String -> Int
strToInt str = read str :: Int

lineToSeq :: String -> [Int]
lineToSeq seq = map strToInt (words seq)
