module Main where
import BehaviorTree


leaf1 :: [Node] -> NodeStatus -> [LastRunStatusTree] -> Environment -> Codes -> StatusCodeRunningchildren
leaf1 children lastStatus childrenTree environment codes
  | head environment == 1 = (Success, (codes ++ [1], []))
  | head environment > 1 = (Success, (codes, []))
  | otherwise = (Failure, (codes ++ [0], []))


leaf2 :: [Node] -> NodeStatus -> [LastRunStatusTree] -> Environment -> Codes -> StatusCodeRunningchildren
leaf2 children lastStatus childrenTree environment codes
  | last environment == 1 = (Success, (codes ++ [2], []))
  | last environment > 1 = (Success, (codes, []))
  | otherwise = (Failure, (codes ++ [3], []))
  
leaf3 :: [Node] -> NodeStatus -> [LastRunStatusTree] -> Environment -> Codes -> StatusCodeRunningchildren
leaf3 children lastStatus childrenTree environment codes
  | last environment == head environment = (Success, (codes ++ [4], []))
  | otherwise = (Failure, (codes ++ [5], []))



main =
  do {
  print result1
  ; print result2
  ; print result3}
  where
    l1 = Node leaf1 []
    l2 = Node leaf2 []
    l3 = Node leaf3 []
    par1 = Node (parallelCreator successOnAllFailureOne) [l1, l2, l3]
    invTree = allInvalid par1
    env1 = [1, 1]
    env2 = [2, 2]
    env3 = [0, 2]
    result1 = evaluateTree par1 invTree env1 []
    result2 = evaluateTree par1 invTree env2 []
    result3 = evaluateTree par1 invTree env3 []
