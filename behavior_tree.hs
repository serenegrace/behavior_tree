module Main where

data NodeStatus = Success | Running | Failure | Invalid
  deriving (Enum, Eq, Show)



type PreviousRunningChildren = [[Int]]

type EnvironmentCodes = [Int]

type Environment = (PreviousRunningChildren, EnvironmentCodes)

type Codes = [Int]

type PairStatusCode = (NodeStatus, Codes)

data Node =  Node
  { evalFunc :: [Node] -> Environment -> Codes -> PairStatusCode,
    children :: [Node]
  }


evaluateTree :: Node -> Environment -> Codes -> PairStatusCode
evaluateTree node = evalFunc node (children node)


selectorFunc :: [Node] -> Environment -> Codes -> PairStatusCode
selectorFunc children environment codes = selectorRunner children environment codes

selectorRunner :: [Node] -> Environment -> Codes -> PairStatusCode
selectorRunner children environment codes
  | null children  = (Failure, codes)
  | otherwise = (newStatus, newCodes)
  where result = evaluateTree (head children) environment codes
        nextResult = if fst result == Failure then selectorRunner (tail children) environment (snd result)
          else (Invalid, codes)
        newStatus = if fst result == Failure then fst nextResult
          else fst result
        newCodes = if fst result == Failure then snd nextResult
          else snd result


sequenceFunc :: [Node] -> Environment -> Codes -> PairStatusCode
sequenceFunc children environment codes = sequenceRunner children environment codes

sequenceRunner :: [Node] -> Environment -> Codes -> PairStatusCode
sequenceRunner children environment codes
  | null children = (Success, codes)
  | otherwise = (newStatus, newCodes)
  where result = evaluateTree (head children) environment codes
        nextResult = if fst result == Success then sequenceRunner (tail children) environment (snd result)
          else (Invalid, codes)
        newStatus = if fst result == Success then fst nextResult
          else fst result
        newCodes = if fst result == Success then snd nextResult
          else snd result


parallelSuccessAllFailureOneFunc :: [Node] -> Environment -> Codes -> PairStatusCode
parallelSuccessAllFailureOneFunc children environment codes = parallelSuccessAllFailureOneRunner children environment codes

parallelSuccessAllFailureOneRunner :: [Node] -> Environment -> Codes -> PairStatusCode
parallelSuccessAllFailureOneRunner children environment codes
  | null children = (Success, codes)
  | otherwise = (newStatus, newCodes)
  where result = evaluateTree (head children) environment codes
        nextResult = parallelSuccessAllFailureOneRunner (tail children) environment (snd result)
        newStatus
          | fst result == Failure || fst nextResult == Failure = Failure
          | fst result == Running || fst nextResult == Running = Running
          | otherwise = Success
        newCodes = if fst result == Success then snd nextResult
          else snd result


parallelSuccessOneFailureOneFunc :: [Node] -> Environment -> Codes -> PairStatusCode
parallelSuccessOneFailureOneFunc children environment codes = parallelSuccessOneFailureOneRunner children environment codes

parallelSuccessOneFailureOneRunner :: [Node] -> Environment -> Codes -> PairStatusCode
parallelSuccessOneFailureOneRunner children environment codes
  | null children = (Running, codes)
  | otherwise = (newStatus, newCodes)
  where result = evaluateTree (head children) environment codes
        nextResult = parallelSuccessOneFailureOneRunner (tail children) environment (snd result)
        newStatus
          | fst result == Failure || fst nextResult == Failure = Failure
          | fst result == Success || fst nextResult == Success = Success
          | otherwise = Running
        newCodes = if fst result == Success then snd nextResult
          else snd result



leaf1 :: [Node] -> Environment -> Codes -> (NodeStatus, Codes)
leaf1 children environment codes
  | head (snd environment) == 1 = (Success, codes ++ [1])
  | head (snd environment) > 1 = (Success, codes)
  | otherwise = (Failure, codes ++ [0])


leaf2 :: [Node] -> Environment -> Codes -> (NodeStatus, Codes)
leaf2 children environment codes
  | last (snd environment) == 1 = (Success, codes ++ [2])
  | last (snd environment) > 1 = (Success, codes)
  | otherwise = (Failure, codes ++ [3])
  



main =
  do {
  print result1
  ; print result2
  ; print result3}
  where
    l1 = Node leaf1 []
    l2 = Node leaf2 []
    sel1 = Node selectorFunc [l1, l2, l1, l2]
    seq1 = Node selectorFunc [l1, l2, l2, l1]
    par1 = Node parallelSuccessAllFailureOneFunc [l1, l2, sel1, seq1]
    par2 = Node parallelSuccessOneFailureOneFunc [par1, sel1, seq1]
    environment1 = ([[]], [1, 3])
    result1 = evaluateTree par2 environment1 []
    environment2 = ([[]], [0, -1])
    result2 = evaluateTree par2 environment2 []
    environment3 = ([[]], [-1, 4])
    result3 = evaluateTree par2 environment2 []
    
    
