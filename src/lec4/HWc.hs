module MyHaskell where

import Data.List (nub,intersperse)

-- Task 1a

data Circuit
  = Input String |
    NOT Circuit |
    AND Circuit Circuit|
    OR Circuit Circuit|
    XOR Circuit Circuit

-- Task 1b

cinput :: String -> Circuit
cinput s = (Input s)

cnot   :: Circuit -> Circuit
cnot = NOT

cand   :: Circuit -> Circuit -> Circuit
cand = AND

cor    :: Circuit -> Circuit -> Circuit
cor = OR

cxor   :: Circuit -> Circuit -> Circuit
cxor = XOR

-- Task 1c

example :: Circuit
example = cor (cand (cinput "x") (cinput "y")) (cxor (cnot (cinput "z")) (cinput "x"))

-- Task 1d

candMany :: [Circuit] -> Circuit
candMany [c] = c
candMany (c:cs) = cand c (candMany cs)

-- Task 2a
--OR(AND(x,y),XOR(NOT(z),x))
instance Show Circuit where
  show (Input x) = x
  show (NOT c) = "NOT(" ++ show c ++ ")"
  show (AND c1 c2) = "AND("++show c1++","++show c2++")"
  show (OR c1 c2) = "OR("++show c1++","++show c2++")"
  show (XOR c1 c2) = "XOR("++show c1++","++show c2++")"

-- Task 2b

simplify :: Circuit -> Circuit
simplify (NOT c) = (NOT (simplify c))
simplify (AND c1 c2) = (AND (simplify c1) (simplify c2))
simplify (OR c1 c2) = (NOT (AND (NOT (simplify c1)) (NOT (simplify c2))))
simplify (XOR c1 c2) = simplify (OR (AND (simplify c1) (NOT (simplify c2))) (AND (NOT (simplify c1)) (simplify c2)))
simplify (Input x) = (Input x)

-- Task 2c

size :: Circuit -> Int
size (NOT c) = 1 + size c
size (AND c1 c2) = 1 + size c1 +size c2
size (OR c1 c2) = 1 + size c1 +size c2
size (XOR c1 c2) = 1 + size c1 +size c2
size _ = 0

-- Task 2d

gateDelay :: Circuit -> Int
gateDelay (Input x) = 0
gateDelay (AND c1 c2) = 1 + max (gateDelay c1) (gateDelay c2)
gateDelay (OR c1 c2) = 1 + max (gateDelay c1) (gateDelay c2)
gateDelay (XOR c1 c2) = 1 + max (gateDelay c1) (gateDelay c2)
gateDelay (NOT c) = 1 + gateDelay c

-- Task 2e

inputs :: Circuit -> [String]
inputs (AND c1 c2) = nub (inputs c1 ++ inputs c2)
inputs (OR c1 c2) = nub (inputs c1 ++ inputs c2)
inputs (XOR c1 c2) = nub (inputs c1 ++ inputs c2)
inputs (NOT c) = nub (inputs c)
inputs (Input x) = [x]

-- Task 3a

simulate :: Circuit -> [(String,Bool)] -> Bool
simulate (Input s) l  = getValue s l where
  getValue::String->[(String,Bool)]->Bool
  getValue x ((string,value):ls) = if x==string then value else getValue x ls
simulate (AND c1 c2) l= (simulate c1 l) && (simulate c2 l)
simulate (OR c1 c2) l= (simulate c1 l) && (simulate c2 l)
simulate (NOT c) l= not (simulate c l)
simulate (XOR c1 c2) l= simulate (simplify (XOR c1 c2)) l

-- Task 3b
-- Task 3b

combinations :: Int -> [[Bool]]
combinations 0 = [[]]
combinations n = let cs = combinations (n-1)
                 in map (False:) cs ++ map (True:) cs

-- Task 3c
tabulate :: Circuit -> IO ()
tabulate c =
  let ns      = inputs c
      header  = concat (intersperse " " ns) ++ " | output"
      line bs = concat (intersperse " " (map toC bs)) ++ " | " ++ toC (simulate c (zip ns bs))
      toC True  = "1"
      toC False = "0"
  in mapM_ putStrLn (header : map line (combinations (length ns)))




-- -- Task 4a
--
-- check :: Circuit -> [(String,Bool)] -> Bool -> Bool
-- check c env r = undefined
--
-- checkAll :: [([(String,Bool)],Bool)] -> Circuit -> Bool
-- checkAll = undefined
--
-- -- Task 4b
--
-- splits :: Int -> [(Int,Int)]
-- splits = undefined
--
-- -- Task 4c
--
-- generate :: [String] -> Int -> [Circuit]
-- generate = undefined
--
-- -- Task 4d
--
-- smallest :: [String] -> [([(String,Bool)],Bool)] -> Circuit
-- smallest = undefined
