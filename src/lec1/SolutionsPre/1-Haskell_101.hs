
module Solution where

-- * Haskell 101
-- ----------------------------------------------------------------------------

double :: Int -> Int
double x = 2 * x

myAbs :: Int -> Int
myAbs x
  | x < 0     = -x
  | otherwise = x

toFahrenheit :: Float -> Float
toFahrenheit c = 1.8*c + 32

fizzbuzz :: Int -> String
fizzbuzz x
  | x `mod` 3 == 0 && x `mod` 5 == 0 = "fizzbuzz"
  | x `mod` 3 == 0 = "fizz"
  | x `mod` 5 == 0 = "buzz"
  | otherwise      = show x

