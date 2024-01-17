import Data.Map.Strict qualified as M
import Data.List.Split (splitOn)
import Data.List (partition)

type Range = (Int, Int)

data Part = Part
    { x :: Range 
    , m ::  Range
    , a :: Range
    , s :: Range
    }
    deriving (Show, Eq)

data Selector = X | M | A | S | Any deriving (Show, Eq) 

data Outcome
    = Accept
    | Reject
    | Refer String
    deriving (Show, Eq)

type Workflow = (Selector, Int -> Bool, Outcome) 

type WorkflowMap = M.Map String [Workflow]

instance Show (Int -> Bool) where 
   show f = "Function" 

main = do 
    contents <- readFile "sample.txt"
    let (rawWorkflows : rawParts : _) = (getLineGroups . lines) contents
    let workMap = (M.fromList . map parseWorkflow) rawWorkflows
    -- let parts = map parsePart rawParts
    let initialPart = Part (1,4000) (1,4000) (1,4000) (1,4000)
    -- let outcomes = map (getOutcome workMap) initialPart
    let tempSol = getOutcome workMap initialPart
    let summed = map partToCombinations tempSol
    print tempSol
    print summed
    print (sum summed)

getOutcome :: WorkflowMap -> Part -> [Part]
getOutcome mp initialPart = getOutcome' mp initialPart "in"

decider :: WorkflowMap -> (Part, Outcome) -> [Part]
decider mp (prt, outcome) = case outcome of 
    Accept -> [prt] 
    Reject -> []
    Refer s -> getOutcome' mp prt s 
            
-- TODO: WORK ON THIS!
getOutcome' :: WorkflowMap -> Part -> String -> [Part] 
getOutcome' mp pt st = 
    let partsAndOutcomes = go (mp M.! st) pt
        accepted = [p | (p,o) <- partsAndOutcomes, o == Accept]
        referred = [x | x@(p,o) <- partsAndOutcomes, o /= Accept && o /= Reject]
    in if  null referred 
      then accepted 
      else accepted ++ concatMap getReferOutcomes referred 
    where 
        getReferOutcomes (p, Refer s) = getOutcome' mp p s 

partToCombinations :: Part -> Int 
partToCombinations p = (rangeToNum . x) p * (rangeToNum . m) p * (rangeToNum . a) p * (rangeToNum . s) p

rangeToNum :: Range -> Int 
rangeToNum (s,e) = e - s + 1 
        


go :: [Workflow] -> Part -> [(Part, Outcome)]
go ((Any, _, o):xs) part = [(part, o)]
go ((s, f, o):xs) part = 
    let selectorFn = selectorToFunction s
        (good, bad) = (applyPartition f . selectorFn) part
        goodPart = partFromRange part s good
        badPart = partFromRange part s bad
    in if null bad then [(goodPart, o)] else (goodPart, o) : go xs badPart

     -- if f part 
     --    then o 
     --    else go xs part

getParts :: [(Part, Outcome)] -> Outcome -> [Part]
getParts xs outcome = [p | (p,o) <- xs, o == outcome]


applyPartition :: (Int -> Bool) -> Range -> (Range, Range)
applyPartition f (start, end) = ((head acc, last acc), (head rej, last rej))
    where (acc, rej) = partition f [start..end]


partFromRange :: Part -> Selector -> Range -> Part 
partFromRange p s r = 
    case s of 
        X -> p {x = r} 
        M -> p {m = r}
        A -> p {a = r} 
        S -> p {s = r}

-- *** PARSING ***

parseWorkflow :: String -> (String, [Workflow])
parseWorkflow str = (n, parseWorkflowBody rest) 
    where (n, rest) = break (=='{') str

parseWorkflowBody :: String -> [Workflow]
parseWorkflowBody str = map instToWorkflow pts
   where pts = (splitOn "," . trimBrackets) str
         instToWorkflow :: String -> (Selector, Int -> Bool, Outcome)
         instToWorkflow s = 
            if ':' `elem` s then parseFunction s else case s of 
                "A" -> (Any, allTrue, Accept)
                "R" -> (Any, allTrue, Reject)
                _ -> (Any, allTrue, Refer s)
            where allTrue = const True

parseFunction :: String -> (Selector, Int -> Bool, Outcome)
parseFunction str = (char, f, (parseOut . tail) out)
    where (cond, out) = break (==':') str
          (char, f) = parseCond cond

parseCond :: String -> (Selector, Int -> Bool)
parseCond str = ((charToSelector . head) str, op ((toInt . drop 2) str))
    where op = if '<' `elem` str then (>) else (<)


toInt :: String -> Int 
toInt str = read str :: Int

parseOut :: String -> Outcome
parseOut str = case str of 
    "A" -> Accept
    "R" -> Reject 
    _ -> Refer str

trimBrackets :: String -> String 
trimBrackets = init . tail

getLineGroups :: [String] -> [[String]]
getLineGroups strs = getLineGroups' strs [] []

getLineGroups' :: [String] -> [String] -> [[String]] -> [[String]]
getLineGroups' [] [] outStrs = outStrs
getLineGroups' [] ys outStrs = outStrs ++ [ys]
getLineGroups' (x : xs) ys outStrs =
  if null x
    then getLineGroups' xs [] (outStrs ++ [ys])
    else getLineGroups' xs (ys ++ [x]) outStrs

charToSelector :: Char -> Selector 
charToSelector c = case c of 
     'x' -> X 
     'm' -> M 
     'a' -> A 
     's' -> S 

selectorToFunction :: Selector -> (Part -> Range)
selectorToFunction sel = case sel of 
    X -> x 
    M -> m 
    A -> a 
    S -> s 
