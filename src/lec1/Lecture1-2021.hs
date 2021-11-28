-- 1. write a function that adds up the integers in a list
--    e.g.
--     add [1,2,3,4,5] = 15
add :: [Int] -> Int
add []     = 0
add (x:xs) = x + add xs

add' l
  | null l  = 0
  | otherwise = head l + add (tail l)


-- 2. write a function that counts the number of elements in a list of integers
--    e.g.
--     count [1,2,3,4,5] = 5

count :: [Int] -> Int
count []     = 0
count (x:xs) = 1 + count xs

-- 3a. write a function that computes the average of a list of integers
--    e.g.
--     average [1,2,3,4,5] = 3

average :: [Int] -> Int
average [] = 0
average l = add l `div` count l

average2 :: [Int] -> Int
average2 [] = 0
average2 l = let (c,s) = addAndCount l
             in s `div` c
  where
     addAndCount :: [Int] -> (Int, Int)
     addAndCount []     = (0,0)
     addAndCount (x:xs) = (1 + c, x + s)
       where
         (c,s) = addAndCount xs

-- data Maybe a = Nothing | Just a

average' :: [Int] -> Maybe Int
average' [] = Nothing
average' l  = Just (add l `div` count l)

average'' :: [Int] -> Double
average'' [] = 0
average'' l = fromIntegral (add l) / fromIntegral (count l)

data Result = Negative | TooBig | Actual Int

-- data Either a b = Left a | Right b

-- Either String a

f x = let y = x + 1
          z = x + 2
      in x * y * z

f' x = x * y * z
  where
    y = z + x
    z = x + 2

-- 3b. ask me about this after writing 3a

-- 4a. write a function that inserts an integer in a sorted list, preserving
--     the sortedness
--     e.g.
--      insert [1,2,7] 4 = [1,2,4,7]

insert :: [Int] -> Int -> [Int]
insert []     y = [y]
insert (x:xs) y
  | x < y      =  x : insert xs y
  | otherwise  =  y : x : xs

-- 4b. use 4a. to implement insertion sort, inserting all elements of 
--     an unsorted list in an empty list to obtained a sorted list
--     e.g.
--      isort [3,1,5,4] = [1,3,4,5]

isort :: [Int] -> [Int]
isort []      =  []
isort (x:xs)  =  insert (isort xs) x

-- 5a. write a function to split a list in two halves
--

split :: [Int] -> ([Int],[Int])
split []     = ([],[])
split [x]    = ([x],[])
split (x1:x2:xs) = (x1:ys,x2:zs)
  where 
    (ys,zs) = split xs

split' []   =  ([],[])
split' (x:xs) = (zs,x:ys)
  where 
    (ys,zs) = split' xs


-- 5b. write a function to merge to sorted lists into a sorted list
--     e.g.
--     merge [1,3,5] [2,4] = [1,2,3,4,5]

merge :: [Int] -> [Int] -> [Int]
merge []     ys      = ys
merge xs     []      = xs
merge (x:xs) (y:ys)
  | x <= y     = x : merge xs (y:ys)
  | otherwise  = y : merge (x:xs) ys

-- HOMEWORK:

-- 5c. use 5a and 5b to write mergesort
--     make sure your function terminates for all inputs
--     e.g. with mergesort [1,2,3,4,5]

-- 6a. create a datatype for representing binary trees where
--     only the leaves carry integer values 
 
-- 6b. create a datatype for representing binary trees where
--     only the internal nodes carry integer values 

-- 6c. write functions to add the values in both kinds of trees, 
--      analogous to 1
