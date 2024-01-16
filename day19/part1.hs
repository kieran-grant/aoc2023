import Data.Map.Strict qualified as M

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

type Workflow = [(Part -> Bool, Outcome)]

type WorkflowMap = M.Map String Workflow

main = do 
    contents <- readFile "sample.txt"
    let ls = lines contents
    print ls

getOutcome :: WorkflowMap -> Part -> Outcome
getOutcome mp pt = getOutcome' mp pt "in"
            
getOutcome' :: WorkflowMap -> Part -> String -> Outcome 
getOutcome' mp pt st = 
    case go (M.lookup mp st) pt  of 
        Accept -> Accept
        Reject -> Reject
        Refer r -> getOutcome' mp pt r
    where 
        go ((f, o):xs) part = 
             if f part 
                then o 
                else go xs part

-- parsing
parseWorkflow :: String -> (String, Workflow)

parsePart :: String -> Part


