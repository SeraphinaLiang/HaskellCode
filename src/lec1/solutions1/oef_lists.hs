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

myProduct :: [Integer] -> Integer
myProduct []     = 1
myProduct (x:xs) = x * myProduct xs

insert :: Int -> [Int] -> [Int]
insert x []     = [x]
insert x (y:ys) = if x < y
                    then x : y : ys
                    else y : insert x ys

myLast :: [Int] -> Int
myLast []    = error "empty list"
myLast [x]   = x
myLast (_:t) = myLast t
