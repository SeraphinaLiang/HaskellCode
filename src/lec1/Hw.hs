module Template where

-- * List Operations
-- ----------------------------------------------------------------------------

myProduct :: [Integer] -> Integer
myProduct []=1
myProduct (x:xs)=x*myProduct xs

insert :: Int -> [Int] -> [Int]
insert x []=[x]
insert x (y:ys)
    |x>y  = y:insert x ys
    |x<=y = x:y:ys

myLast :: [Int] -> Int
myLast [x] = x
myLast (x:xs) = myLast xs
--myLast (_:t) = myLast t

-- * Rock - Paper - Scissors
-- ----------------------------------------------------------------------------

data Move = Rock | Paper | Scissors
  deriving (Eq, Show)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose Scissors = Paper

data Result = Win | Lose | Draw
  deriving (Eq, Show)

outcome :: Move -> Move -> Result
outcome m1 m2
    | beat m1==m2 = Lose
    | lose m1==m2 = Win
    | m1==m2 = Draw

-- * List Operations
-- ----------------------------------------------------------------------------

factorial :: Integer -> Integer
factorial x
    | x<=0 = 1
    | x>0  = product [1..x]

myRepeat :: Int -> Int -> [Int]
myRepeat n x
    | n <= 0 = []
    | n > 0  = x:myRepeat (n-1) x
-- myRepeat n x = [ x | _i <- [1..n] ]

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten list = [ y | x <- list, y <- x]
-- flatten (xs:xxs) = xs ++ flatten xxs

range :: Int -> Int -> [Int]
range low high
    | low > high  = []
--    | low == high = [low]
    | otherwise = [low..high]

sumInts :: Int -> Int -> Int
sumInts low high
    | low > high  = 0
    | otherwise = sum [low..high]

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples _ [] = []
removeMultiples n list = [y | y<-list , y `mod` n /= 0]

-- * List Comprehensions
-- ----------------------------------------------------------------------------

mapLC :: (a -> b) -> [a] -> [b]
mapLC _ []     = []
mapLC f (x:xs) = f x:mapLC f xs

mapLC' :: (a -> b) -> [a] -> [b]
mapLC' f l = [f x | x <- l]

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC _ [] = []
filterLC predicate list = [x | x<-list, predicate x]

filterLC' :: (a -> Bool) -> [a] -> [a]
filterLC' _ [] = []
filterLC' predicate (x:xs)
    | predicate x = x:filterLC' predicate xs
    | otherwise   = filterLC' predicate xs

-- * Folds
-- ----------------------------------------------------------------------------

mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x:xs) = x+mySum xs

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x:xs) = x*myProduct xs

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldInts _ base [] = base
foldInts fn base (x:xs) = fn x (foldInts fn base xs)

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

head' :: [a] -> [a]
head' [x] = []
head' (x:xs) = x:head' xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ base [] = base
myFoldl fn base list = fn (myFoldl fn base (head' list)) (last' list)
--myFoldl  f base (x:xs) = myFoldl f (f base x) xs

--默认
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ base [] = base
myFoldr fn base (x:xs) = fn x (myFoldr fn base xs)

-- 不会
readInBase :: Int -> [Int] -> Int
readInBase _ [] = 0
readInBase base digits = myFoldl (\acc d -> acc*base +d) 0 digits
-- 订正
readInBase :: Int -> [Int] -> Int
readInBase base digits = myFoldl (\acc d -> acc*base + d) 0 digits

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap fn list = [fn x | x<-list]

myMapF :: (a -> b) -> [a] -> [b]
myMapF _ [] = []
myMapF fn (x:xs) = fn x:myMapF fn xs

-- * Function Chaining
-- ----------------------------------------------------------------------------

applyAll :: [a -> a] -> a -> a
applyAll [] v = v
applyAll (f:fx) v = f$applyAll fx v

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n f v
    | n<=0 = v
    | otherwise = applyAll [f| _i<-[1..n] ] v

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs v list = [f v | f<-list]
