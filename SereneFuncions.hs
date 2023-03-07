module SereneFunctions where

sereneXor :: Bool -> Bool -> Bool
sereneXor x y = x /= y

sereneXnor :: Bool -> Bool -> Bool
sereneXnor x y = x == y

sereneImplies :: Bool -> Bool -> Bool
sereneImplies x y = not x || y

sereneCount :: [Bool] -> Int
sereneCount x
  | null x = 0
  | head x = 1 + sereneCount (tail x)
  | otherwise = sereneCount (tail x)
