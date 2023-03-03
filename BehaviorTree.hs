module BehaviorTree where

data NodeStatus = Success | Running | Failure | Invalid
  deriving (Enum, Eq, Show)

data LastRunStatusTree = LastRunStatusTree
  { val :: NodeStatus
  , subTree :: [LastRunStatusTree]
  } deriving (Show)

type Environment = [Int]

type Codes = [Int]

type StatusCodeRunning = (NodeStatus, (Codes, LastRunStatusTree))

type StatusCodeRunningchildren = (NodeStatus, (Codes, [LastRunStatusTree]))

data Memory = TrueMemory | PartialMemory | NoMemory
  deriving (Enum, Eq, Show)

data Node =  Node
  { evalFunc :: [Node] -> NodeStatus -> [LastRunStatusTree] -> Environment -> Codes -> StatusCodeRunningchildren
  , memory :: Memory
  , children :: [Node]
  }
  
----------------------------End of data declarations

statusFromC :: StatusCodeRunningchildren -> NodeStatus
statusFromC = fst

codesFromC :: StatusCodeRunningchildren -> Codes
codesFromC scr = fst (snd scr)

treeFromC :: StatusCodeRunningchildren -> [LastRunStatusTree]
treeFromC scr = snd (snd scr)

statusFrom :: StatusCodeRunning -> NodeStatus
statusFrom = fst

codesFrom :: StatusCodeRunning -> Codes
codesFrom scr = fst (snd scr)

treeFrom :: StatusCodeRunning -> LastRunStatusTree
treeFrom scr = snd (snd scr)

allInvalid :: Node -> LastRunStatusTree
allInvalid node
  | null (children node) = LastRunStatusTree Invalid []
  | otherwise = LastRunStatusTree Invalid (map allInvalid (children node))

----------------------------End of data manipulators



evaluateTree :: Node -> LastRunStatusTree -> Environment -> Codes -> StatusCodeRunning
evaluateTree node tree environment codes = (newStatus, (newCodes, newTree))
  where result = evalFunc node (children node) (val tree) (subTree tree) environment codes
        newStatus = statusFromC result
        newCodes = codesFromC result
        newTree = LastRunStatusTree newStatus (treeFromC result)


selectorFunc :: [Node] -> NodeStatus -> [LastRunStatusTree] -> Environment -> Codes -> StatusCodeRunningchildren
selectorFunc children lastStatus childrenTree environment codes
  | null children  = (Failure, (codes, []))
  | otherwise = (newStatus, (newCodes, newTree))
  where result = evaluateTree (head children) (head childrenTree) environment codes
        nextResult = if statusFrom result == Failure then selectorFunc (tail children) lastStatus (tail childrenTree) environment (codesFrom result)
          else (Invalid, (codes, tail childrenTree))
        newStatus = if statusFrom result == Failure then statusFromC nextResult
          else statusFrom result
        newCodes = if statusFrom result == Failure then codesFromC nextResult
          else codesFrom result
        newTree = treeFrom result : treeFromC nextResult

selectorMemoryFunc :: [Node] -> NodeStatus -> [LastRunStatusTree] -> Environment -> Codes -> StatusCodeRunningchildren
selectorMemoryFunc children lastStatus childrenTree environment codes
  | null children  = (Failure, (codes, []))
  | lastStatus == Running && val (head childrenTree) /= Running = (statusFromC nextResult, (codesFromC nextResult, head childrenTree : treeFromC nextResult))
  | otherwise = (newStatus, (newCodes, newTree))
  where result = evaluateTree (head children) (head childrenTree) environment codes
        nextResult = if statusFrom result == Failure then selectorMemoryFunc (tail children) lastStatus (tail childrenTree) environment (codesFrom result)
          else (Invalid, (codes, tail childrenTree))
        newStatus = if statusFrom result == Failure then statusFromC nextResult
          else statusFrom result
        newCodes = if statusFrom result == Failure then codesFromC nextResult
          else codesFrom result
        newTree = treeFrom result : treeFromC nextResult
        

sequenceFunc :: [Node] -> NodeStatus -> [LastRunStatusTree] -> Environment -> Codes -> StatusCodeRunningchildren
sequenceFunc children lastStatus childrenTree environment codes
  | null children  = (Success, (codes, []))
  | otherwise = (newStatus, (newCodes, newTree))
  where result = evaluateTree (head children) (head childrenTree) environment codes
        nextResult = if statusFrom result == Success then sequenceFunc (tail children) lastStatus (tail childrenTree) environment (codesFrom result)
          else (Invalid, (codes, tail childrenTree))
        newStatus = if statusFrom result == Success then statusFromC nextResult
          else statusFrom result
        newCodes = if statusFrom result == Success then codesFromC nextResult
          else codesFrom result
        newTree = treeFrom result : treeFromC nextResult


parallelBase :: [Node] -> NodeStatus -> [LastRunStatusTree] -> Environment -> Codes -> ([NodeStatus], (Codes, [LastRunStatusTree]))
parallelBase children lastStatus childrenTree environment codes
  | null children = ([], (codes, []))
  | otherwise = (newStatuses, (newCodes, newTree))
  where result = evaluateTree (head children) (head childrenTree) environment codes
        nextResult = parallelBase (tail children) lastStatus (tail childrenTree) environment (fst (snd result))
        newStatuses = statusFrom result : fst nextResult
        newCodes = fst (snd nextResult)
        newTree = treeFrom result : snd (snd nextResult)

parallelCreator :: ([NodeStatus] -> NodeStatus) -> ([Node] -> NodeStatus -> [LastRunStatusTree] -> Environment -> Codes -> StatusCodeRunningchildren)
parallelCreator conditionFunction = parallelFunc
  where parallelFunc children lastStatus childrenTree environment codes = (newStatus, (newCodes, newTree))
          where result = parallelBase children lastStatus childrenTree environment codes
                newStatus = conditionFunction (fst result)
                newCodes = fst (snd result)
                newTree = snd (snd result)
                

isFailure :: NodeStatus -> Bool
isFailure status
  | status == Failure = True
  | otherwise = False

isRunning :: NodeStatus -> Bool
isRunning status
  | status == Running = True
  | otherwise = False

successOnAllFailureOne :: [NodeStatus] -> NodeStatus
successOnAllFailureOne statuses
  | null statuses = Running
  | any isFailure statuses = Failure
  | any isRunning statuses = Running
  | otherwise = Success

