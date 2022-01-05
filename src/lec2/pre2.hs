foldr' :: (Int -> a -> a) -> a -> [Int] -> a
foldr' f z []       = z
foldr' f z (x : xs) = f x (foldr f z xs)

length' :: [Int] -> Int
length' = foldr' add 0 where
   add::Int->Int->Int
   add x len = len+1

any' :: (Int -> Bool) -> [Int] -> Bool
any' p= foldr' decide False where
  decide::Int->Bool->Bool
  decide x q = p x || q

all' :: (Int -> Bool) -> [Int] -> Bool
all' p= foldr' decide True where
  decide::Int->Bool->Bool
  decide x q = p x && q

map' :: (Int -> Int) -> [Int] -> [Int]
map' f= foldr' tomap [] where
  tomap::Int->[Int]->[Int]
  tomap x xs= (f x):xs

filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' p=foldr' fil [] where
  fil::Int -> [Int] ->[Int]
  fil x xs | p x = x:xs | otherwise = xs

-- * Given helpers

even' :: Int -> Bool
even' =even

not' :: Bool -> Bool
not' = not

absolute' :: Int -> Int
absolute' = abs

greaterThanFive :: Int -> Bool
greaterThanFive x = x > 5

-- * Beginning Composer

amountEven :: [Int] -> Int
amountEven = length.(filter even)

onlyOdd :: [Int] -> [Int]
onlyOdd = filter (not.even)

absGtFive :: [Int] -> Int
absGtFive = length.filter greaterThanFive.map' absolute'

anyEvenGtFive :: [Int] -> Bool
anyEvenGtFive = (any even).(filter greaterThanFive)

anyEvenGtFive' :: [Int] -> Bool
anyEvenGtFive' = not.(all (not. even)).(filter greaterThanFive)

