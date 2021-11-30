
module Template where

foldr' :: (Int -> a -> a) -> a -> [Int] -> a
foldr' f z []       = z
foldr' f z (x : xs) = f x (foldr f z xs)

length' :: [Int] -> Int
length' = foldr' add 0
  where
    add :: Int -> Int -> Int
    add _ i = i + 1

any' :: (Int -> Bool) -> [Int] -> Bool
any' p = foldr' decide False
  where
    decide :: Int -> Bool -> Bool
    decide i q = p i || q

all' :: (Int -> Bool) -> [Int] -> Bool
all' p = foldr' decide True
  where
    decide :: Int -> Bool -> Bool
    decide i q = p i && q

map' :: (Int -> Int) -> [Int] -> [Int]
map' f = foldr' appF []
  where
    appF :: Int -> [Int] -> [Int]
    appF i xs = (f i) : xs

filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' p = foldr' concatIfP []
  where
    concatIfP :: Int -> [Int] -> [Int]
    concatIfP i xs
      | p i       = i : xs
      | otherwise = xs

-- * Given helpers

even' :: Int -> Bool
even' = even

not' :: Bool -> Bool
not' = not

absolute' :: Int -> Int
absolute' = abs

greaterThanFive :: Int -> Bool
greaterThanFive x = x > 5

-- * Beginning Composer

amountEven :: [Int] -> Int
amountEven = length . (filter even)

onlyOdd :: [Int] -> [Int]
onlyOdd = filter (not . even)

absGtFive :: [Int] -> Int
absGtFive = length . filter greaterThanFive . map' absolute'

anyEvenGtFive :: [Int] -> Bool
anyEvenGtFive = (any even) . (filter greaterThanFive)

anyEvenGtFive' :: [Int] -> Bool
anyEvenGtFive' = not . (all (not . even)) . (filter greaterThanFive)
