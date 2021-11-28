
module Template where

-- * List Operations
-- ----------------------------------------------------------------------------

count :: [Int] -> Int
count []     = 0
count (_:xs) = 1 + count xs

myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

append :: [Int] -> [Int] -> [Int]
append []     ys = ys
append (x:xs) ys = x : append xs ys
