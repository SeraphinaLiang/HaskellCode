
module TemplateSolution where

import Control.Monad

-- * Lazy is as lazy doesn't
-- ----------------------------------------------------------------------------

odds :: [Int]
odds = [2*i + 1 | i <- [0..]]

strings :: [String]
strings = "" : [x : xs | xs <- strings, x <- "abc"]

pythagorean :: [(Int, Int, Int)]
pythagorean = [ (a,b,c)
              | m <- [2..]
              , n <- [1 .. m-1]
              , odd m /= odd n
              , gcd m n == 1
              , let a = m*m-n*n
              , let b = 2*m*n
              , let c = m*m + n*n]

partialSums :: Num a => [a] -> [a]
partialSums [] = []
partialSums (x:xs) = ps
  where ps = x : zipWith (+) xs ps
