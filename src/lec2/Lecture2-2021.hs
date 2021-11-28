{-
     The universal property of folds
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

     h = foldr c n

     <=>

     h []      = n
     h (x:xs)  = c x (h xs)

     x:xs = (:) x xs

 -}

-- 1. a. Write the identity function on integer lists in terms of foldr
--       e.g.,  idListInt [1,2,3] = [1,2,3]

idListInt :: [a] -> [a]
-- idListInt l = l
idListInt l = foldr (:) [] l

-- id

--    b. What is a more general type for this function?

-- 2. a. rewrite the following functions from last lecture in terms of foldr

addAndCount :: [Int] -> (Int, Int)
addAndCount []     = (0,0)
addAndCount (x:xs) = (1 + c, x + s)
  where
    (c,s) = addAndCount xs

addAndCount' :: [Int] -> (Int, Int)
addAndCount' l = foldr (\x (c,s) -> (1 + c, x + s)) (0,0) l

split :: [Int] -> ([Int],[Int])
split []     = ([],[])
split (x:xs) = (zs,x:ys)
  where
    (ys,zs) = split xs

split' :: [Int] -> ([Int],[Int])
split' l = foldr c n l where
  n = ([],[])
  c x (l1,l2) = (l2, x:l1) -- function

--    b. write the following function with foldr

tuple :: [a] -> (b -> [(a,b)])   --return a function that take in b
tuple []       = \ y ->  []
tuple (x:xs)   = \ y ->  (x,y) : tuple xs y

tuple' :: [a] -> (b -> [(a,b)])
tuple' l = foldr c n l where
  n = \ y ->  []
  c x r = \ y ->  (x,y) : r y -- return a function f(y)


-- 3. a. Write the function tails using foldr.
--       > tails [1..5]
--       [[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5],[]]

--       c 1 [[2,3,4,5],[3,4,5],[4,5],[5],[]] = [[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5],[]]

tails :: [a] -> [[a]]
tails l = foldr c n l where
  n = [[]]
  c x r@(ys:_) =  (x : ys) : r

--    b. Write the function mts (maximum tail sum)
--       that computes the maximum value among
--       the sums of all the tails of a list
--       > mts [1,-10,2,3]
--       5
--       because:
--         sum []          =  0
--         sum [3]         =  3
--         sum [2,3]       =  5 <<<
--         sum [-10,2,3]   = -5
--         sum [1,-10,2,3] = -4
--
--       Make use of:
--         tails   :: [Int] -> [[Int]]
--         sum     :: [Int] -> Int
--         maximum :: [Int] -> Int

mts :: [Int] -> Int
mts = maximum . map sum . tails
-- 1. tails, [[Int]] --2. map sum, [Int]--3. maximum, Int

--    c.*** challenge *** Write mts with a single list traversal

-- 4a. create a datatype for representing binary trees where
--     only the leaves carry values

data Tree a
  = Fork (Tree a) (Tree a)
  | Leaf a

--  b. write a fold function for your tree type that works
--     in a similar way as foldr does for lists

foldTree :: (r -> r -> r)  -> (a -> r) -> Tree a -> r
foldTree f l (Leaf x)      = l x
foldTree f l (Fork lt rt)  = f (foldTree f l lt) (foldTree f l rt)

-- HOMEWORK:

--  c. write a function that adds the values in a tree using
--     your fold function

addup::()
addup l = foldTree c n l where
  n = 0
  c (Leaf x) sum = x+sum
  c (Fork lt rt) sum = addup lt + addup rt

-- 5. a. Can you write this function with foldr?

insert :: [Int] -> Int -> [Int]
insert []     y = [y]
insert (x:xs) y
  | x < y      =  x : insert xs y
  | otherwise  =  y : x : xs

insert' :: [Int]->Int ->[Int]
insert' i l= foldr' f [i] l where
  f::Int->[Int]->[Int]
  f x xs | x<i = x:f i xs | otherwise = i : x : xs

--    b. Propose a generarlization of foldr that
--       allows writing insert.

--    c. How to generalize insert to work also for
--       other orderings on Ints, e.g., descending?
