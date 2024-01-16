import Data.Map.Strict qualified as M
import Data.List.Split (splitOn)
import Data.List (partition)
import Data.Bifunctor (Bifunctor(bimap))

data Part = Part
    { x :: (Int, Int)
    , m :: (Int, Int)
    , a :: (Int, Int)
    , s :: (Int, Int)
    }
    deriving (Show, Eq)

data Outcome
    = Accept
    | Reject
    | Refer String
    deriving (Show, Eq)

type Workflow = (Part -> (Part, Part), Outcome) 

type WorkflowMap = M.Map String [Workflow]

{- 
So for part 2, just put one part through the pipeline 
Use `partition` at each step rather than the bool version 
Then continue for each new branch until it reaches either accept or reject 
Return the part with ranges
 - -}

main = do 
    contents <- readFile "sample.txt"
    let rawWorkflows = (head . getLineGroups . lines) contents
    let workMap = (M.fromList . map parseWorkflow) rawWorkflows
    let initialPart = Part (1, 4000) (1, 4000) (1, 4000) (1, 4000)
    print initialPart
    -- let outcomes = map (getOutcome workMap) parts
    -- let soln = sum [sumPart p | (p, s) <- zip parts outcomes, s == Accept]
    -- print soln

getOutcome :: WorkflowMap -> Part -> Outcome
getOutcome mp pt = getOutcome' mp pt "in"
            
getOutcome' :: WorkflowMap -> Part -> String -> Outcome 
getOutcome' mp pt st = 
    
    case go (mp M.! st) pt  of 
        Accept -> Accept
        Reject -> Reject
        Refer r -> getOutcome' mp pt r
    where 
        (success, fail) = go (mp M.! st) pt
        go ((f, o):xs) part = 
             if f part 
                then o 
                else go xs part

-- sumPart :: Part -> Int 
-- sumPart p = x p + m p + a p + s p

-- *** PARSING ***

parseWorkflow :: String -> (String, [Workflow])
parseWorkflow str = (n, parseWorkflowBody rest) 
    where (n, rest) = break (=='{') str

parseWorkflowBody :: String -> [Workflow]
parseWorkflowBody str = map instToWorkflow pts
   where pts = (splitOn "," . trimBrackets) str
         instToWorkflow :: String -> Workflow
         instToWorkflow s = 
            if ':' `elem` s then parseFunction s else case s of 
                "A" -> (allTrue, Accept)
                "R" -> (allTrue, Reject)
                _ -> (allTrue, Refer s)
            where allTrue = (\x -> (x, ))

parseFunction :: String -> (Part -> (Part,Part), Outcome)
parseFunction str = (parseCond cond, (parseOut . tail) out)
    where (cond, out) = break (==':') str

parseCond :: String -> (Part -> (Part, Part))
parseCond str = generateFunction (head str) (op ((toInt . drop 2) str))
    where op = if '<' `elem` str then (>) else (<)

-- this function should split a range (x,y) 
generateFunction :: Char -> (Int -> Bool) -> Part -> (Part, Part)
generateFunction char fn part = case char of 
    'x' -> (part {x = sucess}, part {x = fail})    
    'm' -> (part {m = sucess}, part {m = fail})    
    'a' -> (part {a = sucess}, part {a = fail})    
    's' -> (part {s = sucess}, part {s = fail})    
    where
        res = (partition fn . manifestRange . f) part
        f = case char of 
            'x' -> x
            'm' -> m
            'a' -> a 
            's' -> s
        (sucess, fail) = bimap listToRange listToRange res


manifestRange :: (Int, Int) -> [Int]
manifestRange (a,b) = [a..b]

listToRange :: [Int] -> (Int, Int)
listToRange ls = (head ls, last ls)

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
