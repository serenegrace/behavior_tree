module Main where
import BehaviorTree


leaf0 :: [Node] -> NodeStatus -> [MemoryTree] -> NodeStatus -> [MemoryTree] -> Environment -> Codes -> NodeOutput
leaf0 children memoryStatus memoryTree partialStatus partialTree environment codes
  | head environment < 0 = (Failure, codes ++ [0], environment, [], [])
  | head environment == 0 = (Running, codes ++ [1], environment, [], [])
  | otherwise = (Success, codes ++ [2], environment, [], [])

leaf1 :: [Node] -> NodeStatus -> [MemoryTree] -> NodeStatus -> [MemoryTree] -> Environment -> Codes -> NodeOutput
leaf1 children memoryStatus memoryTree partialStatus partialTree environment codes
  | head (tail environment) < 0 = (Failure, codes ++ [3], environment, [], [])
  | head (tail environment) == 0 = (Running, codes ++ [4], environment, [], [])
  | otherwise = (Success, codes ++ [5], environment, [], [])

leaf2 :: [Node] -> NodeStatus -> [MemoryTree] -> NodeStatus -> [MemoryTree] -> Environment -> Codes -> NodeOutput
leaf2 children memoryStatus memoryTree partialStatus partialTree environment codes
  | head (tail (tail environment)) < 0 = (Failure, codes ++ [6], environment, [], [])
  | head (tail (tail environment)) == 0 = (Running, codes ++ [7], environment, [], [])
  | otherwise = (Success, codes ++ [8], environment, [], [])

leaf3 :: [Node] -> NodeStatus -> [MemoryTree] -> NodeStatus -> [MemoryTree] -> Environment -> Codes -> NodeOutput
leaf3 children memoryStatus memoryTree partialStatus partialTree environment codes
  | head (tail (tail (tail environment))) < 0 = (Failure, codes ++ [9], environment, [], [])
  | head (tail (tail (tail environment))) == 0 = (Running, codes ++ [10], environment, [], [])
  | otherwise = (Success, codes ++ [11], environment, [], [])

leaf4 :: [Node] -> NodeStatus -> [MemoryTree] -> NodeStatus -> [MemoryTree] -> Environment -> Codes -> NodeOutput
leaf4 children memoryStatus memoryTree partialStatus partialTree environment codes
  | head (tail (tail (tail (tail environment)))) < 0 = (Failure, codes ++ [12], environment, [], [])
  | head (tail (tail (tail (tail environment)))) == 0 = (Running, codes ++ [13], environment, [], [])
  | otherwise = (Success, codes ++ [14], environment, [], [])

cartProd :: [a] -> [b] -> [(a, b)]
cartProd setx sety = [(x, y) | x <- setx, y <- sety]

pairToList pair = [fst pair, snd pair]
listPairToListList = map pairToList

newEval :: Node -> MemoryTree -> MemoryTree -> (Environment -> TreeOutput)
newEval n m p = theNewEval
  where theNewEval = evaluateTree n m p

blankEval :: Node -> (Environment -> TreeOutput)
blankEval n = newEval n invTree invTree
  where invTree = allInvalid n

runAll :: [Node] -> [Environment] -> [TreeOutput]
runAll nodes environments = [blankEval node environment | node <- nodes, environment <- environments]

runAllMemory :: [Node] -> [Environment] -> MemoryTree -> [TreeOutput]
runAllMemory nodes environments memory = [newEval node memory memory environment | node <- nodes, environment <- environments]

pruneSCR :: TreeOutput -> (NodeStatus, [Int])
pruneSCR scr = (getStatus scr, getCodes scr)

main =
  do {
    print part1
    ; print codes1
    ; print codes2
    ; print codes3
    ; print codes4
    ; print codes5
    ; print codes6}
  where l0 = Node leaf0 []
        l1 = Node leaf1 []
        l2 = Node leaf2 []
        l3 = Node leaf3 []
        l4 = Node leaf4 []
        sel = Node selectorPartialFunc [l1, l2, l3]
        seq = Node sequenceFunc [l0, sel, l4]
        mem0 = allInvalid seq
        part0 = mem0
        sfrss = [1, -1, 0, 1, 1]
        (status1, codes1, env1, mem1, part1) = evaluateTree seq mem0 part0 sfrss
        sssss = [1, 1, 1, 1, 1]
        (status2, codes2, env2, mem2, part2) = evaluateTree seq mem1 part1 sssss
        rffff = [0, -1, -1, -1, -1]
        (status3, codes3, env3, mem3, part3) = evaluateTree seq mem1 part1 rffff
        (status4, codes4, env4, mem4, part4) = evaluateTree seq mem3 part3 sssss
        fffff = [-1, -1, -1, -1, -1]
        (status5, codes5, env5, mem5, part5) = evaluateTree seq mem1 part1 fffff
        (status6, codes6, env6, mem6, part6) = evaluateTree seq mem5 part5 sssss
