import Data.Map.Strict qualified as M
import Data.List.Split (splitOn)

data Part = Part
    { x :: Int
    , m :: Int
    , a :: Int
    , s :: Int
    }
    deriving (Show, Eq)

data Outcome
    = Accept
    | Reject
    | Refer String
    deriving (Show, Eq)

type Workflow = (Part -> Bool, Outcome) 

type WorkflowMap = M.Map String [Workflow]

main = do 
    contents <- readFile "input.txt"
    let (rawWorkflows : rawParts : _) = (getLineGroups . lines) contents
    let workMap = (M.fromList . map parseWorkflow) rawWorkflows
    let parts = map parsePart rawParts
    let outcomes = map (getOutcome workMap) parts
    let soln = sum [sumPart p | (p, s) <- zip parts outcomes, s == Accept]
    print soln

getOutcome :: WorkflowMap -> Part -> Outcome
getOutcome mp pt = getOutcome' mp pt "in"
            
getOutcome' :: WorkflowMap -> Part -> String -> Outcome 
getOutcome' mp pt st = 
    case go (mp M.! st) pt  of 
        Accept -> Accept
        Reject -> Reject
        Refer r -> getOutcome' mp pt r
    where 
        go ((f, o):xs) part = 
             if f part 
                then o 
                else go xs part

sumPart :: Part -> Int 
sumPart p = x p + m p + a p + s p

-- *** PARSING ***

parseWorkflow :: String -> (String, [Workflow])
parseWorkflow str = (n, parseWorkflowBody rest) 
    where (n, rest) = break (=='{') str

parseWorkflowBody :: String -> [Workflow]
parseWorkflowBody str = map instToWorkflow pts
   where pts = (splitOn "," . trimBrackets) str
         instToWorkflow :: String -> (Part -> Bool, Outcome)
         instToWorkflow s = 
            if ':' `elem` s then parseFunction s else case s of 
                "A" -> (allTrue, Accept)
                "R" -> (allTrue, Reject)
                _ -> (allTrue, Refer s)
            where allTrue = const True

parseFunction :: String -> (Part -> Bool, Outcome)
parseFunction str = (parseCond cond, (parseOut . tail) out)
    where (cond, out) = break (==':') str

parseCond :: String -> (Part -> Bool)
parseCond str = generateFunction (head str) op ((toInt . drop 2) str) 
    where op = if '<' `elem` str then (>) else (<)

generateFunction :: Char -> (Int -> Int -> Bool) -> Int -> (Part -> Bool)
generateFunction c o n = case c of 
    'x' -> o n . x 
    'm' -> o n . m 
    'a' -> o n . a 
    's' -> o n . s 

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

parsePart :: String -> Part 
parsePart str = Part x' m' a' s'
    where words = (splitOn "," . trimBrackets) str
          (x' : m' : a' : s' : _)= map (toInt . drop 2) words
