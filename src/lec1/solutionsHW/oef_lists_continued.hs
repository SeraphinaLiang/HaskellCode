factorial :: Integer -> Integer
factorial n = product [1..n]

myRepeat :: Int -> Int -> [Int]
myRepeat n x = [ x | _i <- [1..n] ]

-- myRepeat :: Int -> Int -> [Int]
-- myRepeat n x | n <= 0    = []
--              | otherwise = x : myRepeat (n-1) x

flatten :: [[Int]] -> [Int]
flatten xss = [ x | xs <- xss, x <- xs ]

-- flatten :: [[Int]] -> [Int]
-- flatten []       = []
-- flatten (xs:xxs) = xs ++ flatten xxs

range :: Int -> Int -> [Int]
range low high = [low..high]

sumInts :: Int -> Int -> Int
sumInts a b = sum [a..b]

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples x list = [y | y <- list, y `mod` x /= 0]
