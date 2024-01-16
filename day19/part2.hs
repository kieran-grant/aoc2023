import Data.Map.Strict qualified as M
import Data.List.Split (splitOn)

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

main = do 
    contents <- readFile "input.txt"
    let (rawWorkflows : rawParts : _) = (getLineGroups . lines) contents
    let workMap = (M.fromList . map parseWorkflow) rawWorkflows
    -- let parts = map parsePart rawParts
    let initialPart = [Part (1,4000) (1,4000) (1,4000) (1,4000)]
    let outcomes = map (getOutcome workMap) initialPart
    -- let soln = sum [sumPart p | (p, s) <- zip parts outcomes, s == Accept]
    print outcomes

getOutcome :: WorkflowMap -> Part -> Outcome
getOutcome mp pt = getOutcome' mp pt "in"
            
-- TODO: WORK ON THIS!
getOutcome' :: WorkflowMap -> Part -> String -> Outcome 
getOutcome' mp pt st = 
    case go (mp M.! st) pt  of 
        Accept -> Accept
        Reject -> Reject
        Refer r -> getOutcome' mp pt r
    where 
        go ((_, f, o):xs) part  = 
             if f part 
                then o 
                else go xs part

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
     _  -> Any
