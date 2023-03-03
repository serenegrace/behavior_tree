module Main where

data NodeStatus = Success | Running | Failure | Invalid
  deriving (Enum, Eq, Show)

type Environment = Int

type Codes = [Int]

type PairStatusCode = (NodeStatus, Codes)

data Node =  Node
  { evalFunc :: [Node] -> Environment -> Codes -> PairStatusCode,
    children :: [Node]
  }


evaluateTree :: Node -> Environment -> Codes -> PairStatusCode
evaluateTree node = evalFunc node (children node)


selectorFunc :: [Node] -> Environment -> Codes -> PairStatusCode
selectorFunc children environment codes = selectorRunner children environment codes 0

selectorRunner :: [Node] -> Environment -> Codes -> Int -> PairStatusCode
selectorRunner children environment codes childNum
  | childNum < 0 = (Invalid, codes)
  | childNum >= length children = (Failure, codes)
  | otherwise = (newStatus, newCodes)
  where result = evaluateTree (children !! childNum) environment codes
        nextResult = if fst result == Success then (Invalid, codes)
          else selectorRunner children environment (snd result) (childNum + 1)
        newStatus = if fst result == Success then Success
          else fst nextResult
        newCodes = if fst result == Success then snd result
          else snd nextResult



leaf1 :: [Node] -> Environment -> Codes -> (NodeStatus, Codes)
leaf1 children environment codes
  | environment == 1 = (Success, codes ++ [1])
  | environment > 1 = (Success, codes)
  | otherwise = (Failure, codes ++ [0])


leaf2 :: [Node] -> Environment -> Codes -> (NodeStatus, Codes)
leaf2 children environment codes
  | environment == 1 = (Success, codes ++ [2])
  | environment > 1 = (Success, codes)
  | otherwise = (Failure, codes ++ [3])
  



main = print resultFinal
  where
    c1 = Node leaf1 []
    c2 = Node leaf2 []
    tree = Node selectorFunc [c1, c2]
    environment1 = 3
    result1 = evaluateTree tree environment1 []
    environment2 = 1
    result2 = evaluateTree tree environment2 (snd result1)
    environment3 = 0
    resultFinal = evaluateTree tree environment2 (snd result2)
    
    
