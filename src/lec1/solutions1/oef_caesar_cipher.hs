
import Data.Char
import Data.List

-- * Caesar Cipher
-- ----------------------------------------------------------------------------

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c -- otherwise leave as is

encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs]

table :: [Float]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4
        , 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [ percent (count x) n | x <- ['a'..'z'] ]
  where
    count c = length (filter (==c)   xs) -- count the number of appearances of a letter
    n       = length (filter isLower xs) -- total number of lowercase letters in xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ (o-e)*(o-e) / e | (o,e) <- zip os es ]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack s = encode (-factor) s
  where
    table'      = freqs s
    chitable    = [ chisqr (rotate n table') table | n <- [0..25] ]
    minchi      = minimum chitable
    Just factor = elemIndex minchi chitable
